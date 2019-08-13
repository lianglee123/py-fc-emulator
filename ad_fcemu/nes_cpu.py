# -*- coding: utf-8 -*-
import copy
import typing as tp

import config
import log_differ as ld
import nes_file as nf
import nes_game_pad as ngp
import nes_ppu as np
import utils


class Memory(object):
    def __init__(self, ppu: np.NesPPU, pad: ngp.GamePad):
        self.ram: tp.List[int] = [0] * 0x0800
        self.trainer: tp.List[int] = None
        self.prg_rom: tp.List[int] = None
        self.ppu = ppu
        self.pad = pad

    def load_nes(self, nes: nf.NesFile):
        trainer = nes.prg_rom
        if trainer is None:
            trainer = [0] * 512
        rom = nes.prg_rom
        if len(rom) < 32 * 1024:
            rom = rom * 2
        self.prg_rom = rom
        self.trainer = trainer

    def __getitem__(self, addr: int):
        # 处理镜像
        if 0x0800 <= addr <= 0x1FFF:
            # Mirrors of $0000-$07FF
            addr = (addr - 0x0800) % 0x0800
        elif 0x2008 <= addr <= 0x3FFF:
            # Mirrors of $2000-2007 (repeats every 8 bytes)
            addr = 0x2000 + (addr - 0x2008) % 8

        if 0 <= addr <= 0x07ff:
            return self.ram[addr]
        elif 0x2000 <= addr <= 0x2007:
            return self.ppu.read_for_cpu(addr)
        elif 0x4000 <= addr <= 0x4013:
            # 主动忽略
            return 0
        elif addr == 0x4014:
            return self.ppu.read_for_cpu(addr)
        elif addr == 0x4016:
            return self.pad.read_for_cpu()
        elif 0x4015 <= addr <= 0x5fff:
            # 主动忽略
            return 0
        elif 0x7000 <= addr <= 0x71ff:
            return self.trainer[addr]
        elif 0x8000 <= addr <= 0xffff:
            addr -= 0x8000
            return self.prg_rom[addr]
        else:
            raise IndexError('错误的读地址：<{}>'.format(addr))

    def __setitem__(self, addr: int, value: int):
        if value < 0 or value > 2 ** 8 - 1:
            raise ValueError('<{}> 超过 1 字节的取值范围'.format(value))

        # 处理镜像
        if 0x0800 <= addr <= 0x1FFF:
            # Mirrors of $0000-$07FF
            addr = (addr - 0x0800) % 0x0800
        elif 0x2008 <= addr <= 0x3FFF:
            # Mirrors of $2000-2007 (repeats every 8 bytes)
            addr = 0x2000 + (addr - 0x2008) % 8

        if 0 <= addr <= 0x07ff:
            self.ram[addr] = value
        elif 0x2000 <= addr <= 0x2007:
            self.ppu.write_for_cpu(addr, value)
        elif 0x4000 <= addr <= 0x4013:
            # 主动忽略
            pass
        elif addr == 0x4014:
            self.ppu.write_for_cpu(addr, value)
            beg = value * 0x100
            end = value * 0x100 + 0x100
            self.ppu.load_oam(self.ram[beg:end])
        elif 0x4015 <= addr <= 0x5fff:
            # 主动忽略
            pass
        else:
            raise IndexError('错误的写地址：<{}>'.format(addr))


class _Status(object):
    _mapper = {
        'carry': 0,
        'zero': 1,
        'interrupt': 2,
        'decimal': 3,
        'break_command': 4,
        '_ignore': 5,
        'overflow': 6,
        'negative': 7,
    }

    def __init__(self, value: int):
        self.carry = 0
        self.zero = 0
        self.interrupt = 0
        self.decimal = 0
        self.break_command = 0
        self._ignore = 1
        self.overflow = 0
        self.negative = 0
        self._setup(value)

    def __setattr__(self, key, value):
        if key in self._mapper:
            if value not in (0, 1):
                raise ValueError('错误的 bit 值: <{}>'.format(value))
        super().__setattr__(key, value)

    def _setup(self, value):
        for attr, bit in self._mapper.items():
            if attr == '_ignore':
                continue
            v = (value >> bit) & 1
            self.__setattr__(attr, v)

    @property
    def value(self):
        result = 0
        for attr, bit in self._mapper.items():
            v = self.__getattribute__(attr)
            result |= v << bit
        return result

    def set_negative(self, value: int):
        self.negative = (value >> 7) & 1

    def set_zero(self, value: int):
        if value == 0:
            v = 1
        else:
            v = 0
        self.zero = v


class NesCPU(object):
    def __init__(self, memory: Memory):
        self.pc = 0
        self.a = 0
        self.x = 0
        self.y = 0
        self.sp = 0xfd
        self.status = _Status(0x34)
        self.memory = memory
        self.instructions = _InstructionSet(self)
        self.opcodes = {
            0x00: ('BRK', 'IMP'),
            0x01: ('ORA', 'INX'),
            0x02: ('KIL', 'IMP'),
            0x03: ('SLO', 'INX'),
            0x04: ('NOP', 'ZPG'),
            0x05: ('ORA', 'ZPG'),
            0x06: ('ASL', 'ZPG'),
            0x07: ('SLO', 'ZPG'),
            0x08: ('PHP', 'IMP'),
            0x09: ('ORA', 'IMM'),
            0x0A: ('ASL', 'IMP'),
            0x0B: ('ANC', 'IMM'),
            0x0C: ('NOP', 'ABS'),
            0x0D: ('ORA', 'ABS'),
            0x0E: ('ASL', 'ABS'),
            0x0F: ('SLO', 'ABS'),
            0x10: ('BPL', 'REL'),
            0x11: ('ORA', 'INY'),
            0x12: ('KIL', 'IMP'),
            0x13: ('SLO', 'INY'),
            0x14: ('NOP', 'ZPX'),
            0x15: ('ORA', 'ZPX'),
            0x16: ('ASL', 'ZPX'),
            0x17: ('SLO', 'ZPX'),
            0x18: ('CLC', 'IMP'),
            0x19: ('ORA', 'ABY'),
            0x1A: ('NOP', 'IMP'),
            0x1B: ('SLO', 'ABY'),
            0x1C: ('NOP', 'ABX'),
            0x1D: ('ORA', 'ABX'),
            0x1E: ('ASL', 'ABX'),
            0x1F: ('SLO', 'ABX'),
            0x20: ('JSR', 'ABS'),
            0x21: ('AND', 'INX'),
            0x22: ('KIL', 'IMP'),
            0x23: ('RLA', 'INX'),
            0x24: ('BIT', 'ZPG'),
            0x25: ('AND', 'ZPG'),
            0x26: ('ROL', 'ZPG'),
            0x27: ('RLA', 'ZPG'),
            0x28: ('PLP', 'IMP'),
            0x29: ('AND', 'IMM'),
            0x2A: ('ROL', 'IMP'),
            0x2B: ('ANC', 'IMM'),
            0x2C: ('BIT', 'ABS'),
            0x2D: ('AND', 'ABS'),
            0x2E: ('ROL', 'ABS'),
            0x2F: ('RLA', 'ABS'),
            0x30: ('BMI', 'REL'),
            0x31: ('AND', 'INY'),
            0x32: ('KIL', 'IMP'),
            0x33: ('RLA', 'INY'),
            0x34: ('NOP', 'ZPX'),
            0x35: ('AND', 'ZPX'),
            0x36: ('ROL', 'ZPX'),
            0x37: ('RLA', 'ZPX'),
            0x38: ('SEC', 'IMP'),
            0x39: ('AND', 'ABY'),
            0x3A: ('NOP', 'IMP'),
            0x3B: ('RLA', 'ABY'),
            0x3C: ('NOP', 'ABX'),
            0x3D: ('AND', 'ABX'),
            0x3E: ('ROL', 'ABX'),
            0x3F: ('RLA', 'ABX'),
            0x40: ('RTI', 'IMP'),
            0x41: ('EOR', 'INX'),
            0x42: ('KIL', 'IMP'),
            0x43: ('SRE', 'INX'),
            0x44: ('NOP', 'ZPG'),
            0x45: ('EOR', 'ZPG'),
            0x46: ('LSR', 'ZPG'),
            0x47: ('SRE', 'ZPG'),
            0x48: ('PHA', 'IMP'),
            0x49: ('EOR', 'IMM'),
            0x4A: ('LSR', 'IMP'),
            0x4B: ('ASR', 'IMM'),
            0x4C: ('JMP', 'ABS'),
            0x4D: ('EOR', 'ABS'),
            0x4E: ('LSR', 'ABS'),
            0x4F: ('SRE', 'ABS'),
            0x50: ('BVC', 'REL'),
            0x51: ('EOR', 'INY'),
            0x52: ('KIL', 'IMP'),
            0x53: ('SRE', 'INY'),
            0x54: ('NOP', 'ZPX'),
            0x55: ('EOR', 'ZPX'),
            0x56: ('LSR', 'ZPX'),
            0x57: ('SRE', 'ZPX'),
            0x58: ('CLI', 'IMP'),
            0x59: ('EOR', 'ABY'),
            0x5A: ('NOP', 'IMP'),
            0x5B: ('SRE', 'ABY'),
            0x5C: ('NOP', 'ABX'),
            0x5D: ('EOR', 'ABX'),
            0x5E: ('LSR', 'ABX'),
            0x5F: ('SRE', 'ABX'),
            0x60: ('RTS', 'IMP'),
            0x61: ('ADC', 'INX'),
            0x62: ('KIL', 'IMP'),
            0x63: ('RRA', 'INX'),
            0x64: ('NOP', 'ZPG'),
            0x65: ('ADC', 'ZPG'),
            0x66: ('ROR', 'ZPG'),
            0x67: ('RRA', 'ZPG'),
            0x68: ('PLA', 'IMP'),
            0x69: ('ADC', 'IMM'),
            0x6A: ('ROR', 'IMP'),
            0x6B: ('ARR', 'IMM'),
            0x6C: ('JMP', 'IND'),
            0x6D: ('ADC', 'ABS'),
            0x6E: ('ROR', 'ABS'),
            0x6F: ('RRA', 'ABS'),
            0x70: ('BVS', 'REL'),
            0x71: ('ADC', 'INY'),
            0x72: ('KIL', 'IMP'),
            0x73: ('RRA', 'INY'),
            0x74: ('NOP', 'ZPX'),
            0x75: ('ADC', 'ZPX'),
            0x76: ('ROR', 'ZPX'),
            0x77: ('RRA', 'ZPX'),
            0x78: ('SEI', 'IMP'),
            0x79: ('ADC', 'ABY'),
            0x7A: ('NOP', 'IMP'),
            0x7B: ('RRA', 'ABY'),
            0x7C: ('NOP', 'ABX'),
            0x7D: ('ADC', 'ABX'),
            0x7E: ('ROR', 'ABX'),
            0x7F: ('RRA', 'ABX'),
            0x80: ('NOP', 'IMM'),
            0x81: ('STA', 'INX'),
            0x82: ('NOP', 'IMM'),
            0x83: ('SAX', 'INX'),
            0x84: ('STY', 'ZPG'),
            0x85: ('STA', 'ZPG'),
            0x86: ('STX', 'ZPG'),
            0x87: ('SAX', 'ZPG'),
            0x88: ('DEY', 'IMP'),
            0x89: ('NOP', 'IMM'),
            0x8A: ('TXA', 'IMP'),
            0x8B: ('XAA', 'IMM'),
            0x8C: ('STY', 'ABS'),
            0x8D: ('STA', 'ABS'),
            0x8E: ('STX', 'ABS'),
            0x8F: ('SAX', 'ABS'),
            0x90: ('BCC', 'REL'),
            0x91: ('STA', 'INY'),
            0x92: ('KIL', 'IMP'),
            0x93: ('AHX', 'INY'),
            0x94: ('STY', 'ZPX'),
            0x95: ('STA', 'ZPX'),
            0x96: ('STX', 'ZPY'),
            0x97: ('SAX', 'ZPY'),
            0x98: ('TYA', 'IMP'),
            0x99: ('STA', 'ABY'),
            0x9A: ('TXS', 'IMP'),
            0x9B: ('TAS', 'ABY'),
            0x9C: ('SHY', 'ABX'),
            0x9D: ('STA', 'ABX'),
            0x9E: ('SHX', 'ABY'),
            0x9F: ('AHX', 'ABY'),
            0xA0: ('LDY', 'IMM'),
            0xA1: ('LDA', 'INX'),
            0xA2: ('LDX', 'IMM'),
            0xA3: ('LAX', 'INX'),
            0xA4: ('LDY', 'ZPG'),
            0xA5: ('LDA', 'ZPG'),
            0xA6: ('LDX', 'ZPG'),
            0xA7: ('LAX', 'ZPG'),
            0xA8: ('TAY', 'IMP'),
            0xA9: ('LDA', 'IMM'),
            0xAA: ('TAX', 'IMP'),
            0xAB: ('LAX', 'IMM'),
            0xAC: ('LDY', 'ABS'),
            0xAD: ('LDA', 'ABS'),
            0xAE: ('LDX', 'ABS'),
            0xAF: ('LAX', 'ABS'),
            0xB0: ('BCS', 'REL'),
            0xB1: ('LDA', 'INY'),
            0xB2: ('KIL', 'IMP'),
            0xB3: ('LAX', 'INY'),
            0xB4: ('LDY', 'ZPX'),
            0xB5: ('LDA', 'ZPX'),
            0xB6: ('LDX', 'ZPY'),
            0xB7: ('LAX', 'ZPY'),
            0xB8: ('CLV', 'IMP'),
            0xB9: ('LDA', 'ABY'),
            0xBA: ('TSX', 'IMP'),
            0xBB: ('LAS', 'ABY'),
            0xBC: ('LDY', 'ABX'),
            0xBD: ('LDA', 'ABX'),
            0xBE: ('LDX', 'ABY'),
            0xBF: ('LAX', 'ABY'),
            0xC0: ('CPY', 'IMM'),
            0xC1: ('CMP', 'INX'),
            0xC2: ('NOP', 'IMM'),
            0xC3: ('DCP', 'INX'),
            0xC4: ('CPY', 'ZPG'),
            0xC5: ('CMP', 'ZPG'),
            0xC6: ('DEC', 'ZPG'),
            0xC7: ('DCP', 'ZPG'),
            0xC8: ('INY', 'IMP'),
            0xC9: ('CMP', 'IMM'),
            0xCA: ('DEX', 'IMP'),
            0xCB: ('AXS', 'IMM'),
            0xCC: ('CPY', 'ABS'),
            0xCD: ('CMP', 'ABS'),
            0xCE: ('DEC', 'ABS'),
            0xCF: ('DCP', 'ABS'),
            0xD0: ('BNE', 'REL'),
            0xD1: ('CMP', 'INY'),
            0xD2: ('KIL', 'IMP'),
            0xD3: ('DCP', 'INY'),
            0xD4: ('NOP', 'ZPX'),
            0xD5: ('CMP', 'ZPX'),
            0xD6: ('DEC', 'ZPX'),
            0xD7: ('DCP', 'ZPX'),
            0xD8: ('CLD', 'IMP'),
            0xD9: ('CMP', 'ABY'),
            0xDA: ('NOP', 'IMP'),
            0xDB: ('DCP', 'ABY'),
            0xDC: ('NOP', 'ABX'),
            0xDD: ('CMP', 'ABX'),
            0xDE: ('DEC', 'ABX'),
            0xDF: ('DCP', 'ABX'),
            0xE0: ('CPX', 'IMM'),
            0xE1: ('SBC', 'INX'),
            0xE2: ('NOP', 'IMM'),
            0xE3: ('ISB', 'INX'),
            0xE4: ('CPX', 'ZPG'),
            0xE5: ('SBC', 'ZPG'),
            0xE6: ('INC', 'ZPG'),
            0xE7: ('ISB', 'ZPG'),
            0xE8: ('INX', 'IMP'),
            0xE9: ('SBC', 'IMM'),
            0xEA: ('NOP', 'IMP'),
            0xEB: ('SBC', 'IMM'),
            0xEC: ('CPX', 'ABS'),
            0xED: ('SBC', 'ABS'),
            0xEE: ('INC', 'ABS'),
            0xEF: ('ISB', 'ABS'),
            0xF0: ('BEQ', 'REL'),
            0xF1: ('SBC', 'INY'),
            0xF2: ('KIL', 'IMP'),
            0xF3: ('ISB', 'INY'),
            0xF4: ('NOP', 'ZPX'),
            0xF5: ('SBC', 'ZPX'),
            0xF6: ('INC', 'ZPX'),
            0xF7: ('ISB', 'ZPX'),
            0xF8: ('SED', 'IMP'),
            0xF9: ('SBC', 'ABY'),
            0xFA: ('NOP', 'IMP'),
            0xFB: ('ISB', 'ABY'),
            0xFC: ('NOP', 'ABX'),
            0xFD: ('SBC', 'ABX'),
            0xFE: ('INC', 'ABX'),
            0xFF: ('ISB', 'ABX'),
        }

    def __setattr__(self, key, value):
        if key == 'PC':
            if value < 0 or value > 2 ** 16 - 1:
                raise ValueError('<{}> 超过 <{}> 寄存器的取值范围'.format(value, key))
        elif key in ('A', 'X', 'Y', 'S'):
            if value < 0 or value > 2 ** 8 - 1:
                raise ValueError('<{}> 超过 <{}> 寄存器的取值范围'.format(value, key))
        elif key == 'status':
            if not isinstance(value, _Status):
                raise ValueError('<{}> 不是正确的 Status 寄存器'.format(value))
        super().__setattr__(key, value)

    def dump_registers(self):
        d = dict(
            PC=self.pc,
            P=self.status.value,
            A=self.a,
            X=self.x,
            Y=self.y,
            S=self.sp,
        )
        return d

    def next_mem_value(self):
        pc = self.pc
        v = self.memory[pc]
        self.pc = pc + 1
        return v

    def address_from_mode(self, mode: str):
        if mode == 'IMP':
            return None
        elif mode == 'IMM':
            a = self.next_mem_value()
            return a
        elif mode == 'ABS':
            al = self.next_mem_value()
            ah = self.next_mem_value()
            a = utils.number_from_bytes([al, ah])
            return a
        elif mode == 'ZPG':
            a = self.next_mem_value()
            return a
        elif mode == 'ABX':
            al = self.next_mem_value()
            ah = self.next_mem_value()
            a = utils.number_from_bytes([al, ah])
            i = self.x
            return (a + i) % 0x10000
        elif mode == 'ABY':
            al = self.next_mem_value()
            ah = self.next_mem_value()
            a = utils.number_from_bytes([al, ah])
            i = self.y
            return (a + i) % 0x10000
        elif mode == 'ZPX':
            a = self.next_mem_value()
            i = self.x
            return (a + i) % 0x100
        elif mode == 'ZPY':
            a = self.next_mem_value()
            i = self.y
            return (a + i) % 0x100
        elif mode == 'IND':
            tal = self.next_mem_value()
            tah = self.next_mem_value()
            ta = utils.number_from_bytes([tal, tah])
            # 模拟 6502 的 BUG
            ta2 = (ta & 0xFF00) | ((ta + 1) & 0x00FF)

            al = self.memory[ta]
            ah = self.memory[ta2]
            a = utils.number_from_bytes([al, ah])

            return a
        elif mode == 'INX':
            t = self.next_mem_value()
            i = self.x
            ta = (t + i) % 0x100
            ta2 = (ta + 1) % 0x100

            al = self.memory[ta]
            ah = self.memory[ta2]
            a = utils.number_from_bytes([al, ah])

            return a
        elif mode == 'INY':
            ta = self.next_mem_value()
            ta2 = (ta + 1) % 0x100

            al = self.memory[ta]
            ah = self.memory[ta2]
            a = utils.number_from_bytes([al, ah])

            i = self.y
            return (a + i) % 0x10000
        elif mode == 'REL':
            diff = self.next_mem_value()
            diff = utils.number_from_bytes([diff], signed=True)
            pc = self.pc
            return (pc + diff) % 0x10000
        else:
            raise ValueError('错误的寻址模式：<{}>'.format(mode))

    def execute(self):
        # for debug
        info = {}
        if config.DEBUG:
            info.update(self.dump_registers())

        op, addr, mode = self._prepare()

        if config.DEBUG:
            info['op'] = op
            info['address'] = addr if addr is not None else -1
            utils.log(ld.LogDiffer.log_line_from_info(info))

        self._execute(op, addr, mode)

    def _prepare(self):
        c = self.next_mem_value()
        op, mode = self.opcodes[c]
        addr = self.address_from_mode(mode)
        return op, addr, mode

    def _execute(self, op: str, addr: tp.Optional[int], mode: str):
        self.instructions[op](addr, mode)

    def push(self, value: int):
        sp = self.sp
        addr = sp + 0x0100
        self.memory[addr] = value
        self.sp = sp - 1

    def pop(self):
        sp = self.sp
        sp += 1
        addr = sp + 0x0100
        self.sp = sp
        v = self.memory[addr]
        return v

    def interrupt(self, name: str):
        if name == 'NMI':
            # 将 pc 和 p 压栈
            pc = self.pc
            v = pc
            self.push((v & 0xff00) >> 8)
            self.push(v & 0x00ff)
            # 只有「被压入栈」的 status 的 B flag 被置为 1
            s = copy.copy(self.status)
            s.break_command = 1
            self.push(s.value)
            al_pos = 0xfffa
        elif name == 'RESET':
            al_pos = 0xfffc
        else:
            raise ValueError('错误的 interrupt： <{}>'.format(name))

        al = self.memory[al_pos]
        ah = self.memory[al_pos + 1]
        addr = utils.number_from_bytes([al, ah])
        self.pc = addr


class _InstructionSet(object):
    def __init__(self, cpu: NesCPU):
        self.cpu = cpu

    def __getitem__(self, item):
        return self.__getattribute__(item)

    def value_from_address(self, address: tp.Optional[int], mode: str):
        cpu = self.cpu
        if mode in ('IMM', 'IMP'):
            # 立即寻址 和 隐含寻址 情况
            return address
        else:
            return cpu.memory[address]

    def JMP(self, address: int, mode: str):
        cpu = self.cpu
        cpu.pc = address

    def LDX(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def STX(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.x
        cpu.memory[address] = v

    def JSR(self, address: int, mode: str):
        cpu = self.cpu
        pc = cpu.pc
        v = pc - 1
        cpu.push((v & 0xff00) >> 8)
        cpu.push(v & 0x00ff)
        cpu.pc = address

    def NOP(self, address: int, mode: str):
        # do nothing
        pass

    def SEC(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.carry = 1

    def BCS(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.carry == 1:
            cpu.pc = address

    def CLC(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.carry = 0

    def BCC(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.carry == 0:
            cpu.pc = address

    def LDA(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def BEQ(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.zero == 1:
            cpu.pc = address

    def BNE(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.zero == 0:
            cpu.pc = address

    def STA(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.a
        cpu.memory[address] = v

    def BIT(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        a = cpu.a
        cpu.status.overflow = (v >> 6) & 1
        cpu.status.set_negative(v)
        cpu.status.set_zero(v & a)

    def BVS(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.overflow == 1:
            cpu.pc = address

    def BVC(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.overflow == 0:
            cpu.pc = address

    def BPL(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.negative == 0:
            cpu.pc = address

    def RTS(self, address: int, mode: str):
        cpu = self.cpu
        vl = cpu.pop()
        vh = cpu.pop()
        v = utils.number_from_bytes([vl, vh])
        pc = v + 1
        cpu.pc = pc

    def SEI(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.interrupt = 1

    def SED(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.decimal = 1

    def PHP(self, address: int, mode: str):
        cpu = self.cpu
        # 只有「被压入栈」的 status 的 B flag 被置为 1
        s = copy.copy(cpu.status)
        s.break_command = 1
        cpu.push(s.value)

    def PLA(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.pop()
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def AND(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.a
        v = r & v
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def CMP(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.a
        v = r - v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int(v >= 0)

    def CLD(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.decimal = 0

    def PHA(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.a
        cpu.push(v)

    def PLP(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.pop()
        s = _Status(v)
        # 从栈里弹出的值，外面不会作用到 P 的 B flag 上
        s.break_command = cpu.status.break_command
        cpu.status = s

    def BMI(self, address: int, mode: str):
        cpu = self.cpu
        if cpu.status.negative == 1:
            cpu.pc = address

    def ORA(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.a
        v = r | v
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def CLV(self, address: int, mode: str):
        cpu = self.cpu
        cpu.status.overflow = 0

    def EOR(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.a
        v = r ^ v
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def ADC(self, address: int, mode: str):
        cpu = self.cpu
        mvalue = self.value_from_address(address, mode)
        v = mvalue
        r = cpu.a
        c = cpu.status.carry
        v = r + v + c
        # C flag: set if overflow
        if v > 255:
            v -= 256
            cpu.status.carry = 1
        else:
            cpu.status.carry = 0
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        # 处理 v flag
        v = utils.number_from_bytes([mvalue], signed=True)
        r = utils.number_from_bytes([r], signed=True)
        v = r + v + c
        cpu.status.overflow = int(v > 128 or v < -127)

    def LDY(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        cpu.y = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def CPY(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.y
        v = r - v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int(v >= 0)

    def CPX(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        r = cpu.x
        v = r - v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int(v >= 0)

    def SBC(self, address: int, mode: str):
        cpu = self.cpu
        mvalue = self.value_from_address(address, mode)
        v = mvalue
        r = cpu.a
        c = cpu.status.carry
        v = r - v - (1 - c)
        # C flag: clear if overflow
        if v < 0:
            v += 256
            cpu.status.carry = 0
        else:
            cpu.status.carry = 1
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        # 处理 v flag
        v = utils.number_from_bytes([mvalue], signed=True)
        r = utils.number_from_bytes([r], signed=True)
        v = r - v - (1 - c)
        cpu.status.overflow = int(v > 128 or v < -127)

    def INY(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.y
        v += 1
        v %= 256
        cpu.y = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def INX(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.x
        v += 1
        v %= 256
        cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def DEY(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.y
        v -= 1
        v %= 256
        cpu.y = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def DEX(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.x
        v -= 1
        v %= 256
        cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TAY(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.a
        cpu.y = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TAX(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.a
        cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TYA(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.y
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TXA(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.x
        cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TSX(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.sp
        cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def TXS(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.x
        cpu.sp = v

    def RTI(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.pop()
        s = _Status(v)
        # 从栈里弹出的值，外面不会作用到 P 的 B flag 上
        s.break_command = cpu.status.break_command
        cpu.status = s

        vl = cpu.pop()
        vh = cpu.pop()
        v = utils.number_from_bytes([vl, vh])
        # 这里不需要像 RTS 一样 +1
        pc = v
        cpu.pc = pc

    def LSR(self, address: int, mode: str):
        cpu = self.cpu
        if address is not None:
            old_v = self.value_from_address(address, mode)
            v = old_v >> 1
            cpu.memory[address] = v
        else:
            old_v = cpu.a
            v = old_v >> 1
            cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int(old_v & 1)

    def ASL(self, address: int, mode: str):
        cpu = self.cpu
        if address is not None:
            old_v = self.value_from_address(address, mode)
            v = old_v << 1
            v %= 256
            cpu.memory[address] = v
        else:
            old_v = cpu.a
            v = old_v << 1
            v %= 256
            cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int((old_v >> 7) & 1)

    def ROR(self, address: int, mode: str):
        cpu = self.cpu
        c = cpu.status.carry
        if address is not None:
            old_v = self.value_from_address(address, mode)
            v = (old_v >> 1) + (c * 128)
            cpu.memory[address] = v
        else:
            old_v = cpu.a
            v = (old_v >> 1) + (c * 128)
            cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int(old_v & 1)

    def ROL(self, address: int, mode: str):
        cpu = self.cpu
        c = cpu.status.carry
        if address is not None:
            old_v = self.value_from_address(address, mode)
            v = (old_v << 1) + c
            v %= 256
            cpu.memory[address] = v
        else:
            old_v = cpu.a
            v = (old_v << 1) + c
            v %= 256
            cpu.a = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)
        cpu.status.carry = int((old_v >> 7) & 1)

    def STY(self, address: int, mode: str):
        cpu = self.cpu
        v = cpu.y
        cpu.memory[address] = v

    def INC(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        v += 1
        v %= 256
        cpu.memory[address] = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def DEC(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        v -= 1
        v %= 256
        cpu.memory[address] = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def LAX(self, address: int, mode: str):
        cpu = self.cpu
        v = self.value_from_address(address, mode)
        cpu.a = cpu.x = v
        cpu.status.set_negative(v)
        cpu.status.set_zero(v)

    def SAX(self, address: int, mode: str):
        cpu = self.cpu
        v1, v2 = cpu.a, cpu.x
        v = v1 & v2
        cpu.memory[address] = v

    def DCP(self, address: int, mode: str):
        self.DEC(address, mode)
        self.CMP(address, mode)

    def ISB(self, address: int, mode: str):
        self.ISC(address, mode)

    def ISC(self, address: int, mode: str):
        self.INC(address, mode)
        self.SBC(address, mode)

    def SLO(self, address: int, mode: str):
        self.ASL(address, mode)
        self.ORA(address, mode)

    def RLA(self, address: int, mode: str):
        self.ROL(address, mode)
        self.AND(address, mode)

    def SRE(self, address: int, mode: str):
        self.LSR(address, mode)
        self.EOR(address, mode)

    def RRA(self, address: int, mode: str):
        self.ROR(address, mode)
        self.ADC(address, mode)
