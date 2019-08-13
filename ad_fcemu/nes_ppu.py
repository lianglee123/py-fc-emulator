# -*- coding: utf-8 -*-
import typing as tp

import pygame as pg

import nes_file as nf


class RegisterStore(object):
    def __init__(self):
        self.ppuctrl = 0
        self.ppumask = 0
        self.ppustatus = 0b10100000
        self.oamaddr = 0
        self.oamdata = 0
        self.ppuscroll = 0
        self.ppuaddr = 0
        self.ppudata = 0
        self.oamdma = 0

    def __setattr__(self, key, value):
        key = key.lower()
        if key in ('ppuaddr', 'ppuscroll'):
            if value < 0 or value > 2 ** 16 - 1:
                raise ValueError('<{}> 超过 <{}> 寄存器的取值范围'.format(value, key))
        else:
            if value < 0 or value > 2 ** 8 - 1:
                raise ValueError('<{}> 超过 <{}> 寄存器的取值范围'.format(value, key))
        super().__setattr__(key, value)

    def __getattr__(self, item):
        item = item.lower()
        return super().__getattribute__(item)

    def to_dict(self):
        # for diff logger
        d = {k.upper(): v for k, v in self.__dict__.items()}
        return d


class NesPPU(object):
    def __init__(self, canvas: pg.SurfaceType):
        self.reg = RegisterStore()
        self.memory: tp.List[int] = None
        # 用于判断 PPUSCROLL、PPUADDR 的寄存是该写「高位」还是「低位」
        self.reg_flag: tp.Dict[str, bool] = None
        # 将 cpu 读写的 port 映射到 ppu 寄存器上
        self.reg_mapper_for_cpu: tp.Dict[int, str] = None
        self.buffer = 0
        self.canvas = canvas
        self.colors: tp.List[tp.Tuple[int, int, int, int]] = None
        self.oam: tp.List[int] = None
        self.pattern_table: tp.List[int] = None
        self.palette: tp.List[int] = None

        self.setup()

    def setup(self):
        self.reg_mapper_for_cpu = {
            0x2000: 'PPUCTRL',
            0x2001: 'PPUMASK',
            0x2002: 'PPUSTATUS',
            0x2003: 'OAMADDR',
            0x2004: 'OAMDATA',
            0x2005: 'PPUSCROLL',
            0x2006: 'PPUADDR',
            0x2007: 'PPUDATA',
            0x4014: 'OAMDMA',
        }
        self.memory = [0] * 0x4000
        self.reg_flag = {
            'PPUSCROLL': True,
            'PPUADDR': True,
        }
        self.colors = [
            (0x7F, 0x7F, 0x7F, 0xFF), (0x20, 0x00, 0xB0, 0xFF), (0x28, 0x00, 0xB8, 0xFF), (0x60, 0x10, 0xA0, 0xFF),
            (0x98, 0x20, 0x78, 0xFF), (0xB0, 0x10, 0x30, 0xFF), (0xA0, 0x30, 0x00, 0xFF), (0x78, 0x40, 0x00, 0xFF),
            (0x48, 0x58, 0x00, 0xFF), (0x38, 0x68, 0x00, 0xFF), (0x38, 0x6C, 0x00, 0xFF), (0x30, 0x60, 0x40, 0xFF),
            (0x30, 0x50, 0x80, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF),

            (0xBC, 0xBC, 0xBC, 0xFF), (0x40, 0x60, 0xF8, 0xFF), (0x40, 0x40, 0xFF, 0xFF), (0x90, 0x40, 0xF0, 0xFF),
            (0xD8, 0x40, 0xC0, 0xFF), (0xD8, 0x40, 0x60, 0xFF), (0xE0, 0x50, 0x00, 0xFF), (0xC0, 0x70, 0x00, 0xFF),
            (0x88, 0x88, 0x00, 0xFF), (0x50, 0xA0, 0x00, 0xFF), (0x48, 0xA8, 0x10, 0xFF), (0x48, 0xA0, 0x68, 0xFF),
            (0x40, 0x90, 0xC0, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF),

            (0xFF, 0xFF, 0xFF, 0xFF), (0x60, 0xA0, 0xFF, 0xFF), (0x50, 0x80, 0xFF, 0xFF), (0xA0, 0x70, 0xFF, 0xFF),
            (0xF0, 0x60, 0xFF, 0xFF), (0xFF, 0x60, 0xB0, 0xFF), (0xFF, 0x78, 0x30, 0xFF), (0xFF, 0xA0, 0x00, 0xFF),
            (0xE8, 0xD0, 0x20, 0xFF), (0x98, 0xE8, 0x00, 0xFF), (0x70, 0xF0, 0x40, 0xFF), (0x70, 0xE0, 0x90, 0xFF),
            (0x60, 0xD0, 0xE0, 0xFF), (0x60, 0x60, 0x60, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF),

            (0xFF, 0xFF, 0xFF, 0xFF), (0x90, 0xD0, 0xFF, 0xFF), (0xA0, 0xB8, 0xFF, 0xFF), (0xC0, 0xB0, 0xFF, 0xFF),
            (0xE0, 0xB0, 0xFF, 0xFF), (0xFF, 0xB8, 0xE8, 0xFF), (0xFF, 0xC8, 0xB8, 0xFF), (0xFF, 0xD8, 0xA0, 0xFF),
            (0xFF, 0xF0, 0x90, 0xFF), (0xC8, 0xF0, 0x80, 0xFF), (0xA0, 0xF0, 0xA0, 0xFF), (0xA0, 0xFF, 0xC8, 0xFF),
            (0xA0, 0xFF, 0xF0, 0xFF), (0xA0, 0xA0, 0xA0, 0xFF), (0x00, 0x00, 0x00, 0xFF), (0x00, 0x00, 0x00, 0xFF),
        ]
        self.oam = [0] * 256

    def load_nes(self, nes: nf.NesFile):
        self.memory[:0x2000] = nes.chr_rom

    def mem_value(self, addr: int):
        # 镜像
        if 0x3000 <= addr <= 0x3eff:
            addr -= 0x1000
        elif 0x3f20 <= addr <= 0x3fff:
            addr -= 0x0020
        elif addr in (0x3F10, 0x3F14, 0x3F18, 0x3F1C):
            addr -= 0x0010

        return self.memory[addr]

    def set_mem_value(self, addr: int, value: int):
        if value < 0 or value > 2 ** 8 - 1:
            raise ValueError('<{}>超过了 memory 的取值范围'.format(value))

        # 镜像
        if 0x3000 <= addr <= 0x3eff:
            addr -= 0x1000
        elif 0x3f20 <= addr <= 0x3fff:
            addr -= 0x0020
        elif addr in (0x3F10, 0x3F14, 0x3F18, 0x3F1C):
            addr -= 0x0010

        self.memory[addr] = value

    def load_oam(self, data: tp.List[int]):
        if len(data) != 256:
            raise ValueError('<{}>长度与 OAM 不匹配'.format(data))
        self.oam = data

    def read_for_cpu(self, addr: int):
        name = self.reg_mapper_for_cpu[addr]
        reg = self.reg
        if name == 'PPUDATA':
            c = reg.ppuctrl
            if c & 0b00000100 == 0:
                step = 1
            else:
                step = 32

            a = reg.ppuaddr

            if 0x3F00 <= a <= 0x3FFF:
                v = self.mem_value(a)
                self.buffer = v
            else:
                # NOTE: 这里是直接返回「当前」缓冲区的值，然后再更新缓冲区
                v = self.buffer
                self.buffer = self.mem_value(a)

            reg.ppuaddr = a + step
            return v
        elif name == 'PPUSTATUS':
            v = reg.ppustatus
            self.reg.ppustatus = v & 0b01111111
            return v
        elif name == 'OAMDATA':
            a = reg.oamaddr
            v = self.oam[a]
            reg.oamaddr = a + 1
            return v
        else:
            return getattr(reg, name)

    def write_for_cpu(self, addr: int, value: int):
        name = self.reg_mapper_for_cpu[addr]
        reg = self.reg
        reg_flag = self.reg_flag
        if name == 'PPUDATA':
            c = reg.ppuctrl
            if c & 0b00000100 == 0:
                step = 1
            else:
                step = 32

            a = reg.ppuaddr
            self.set_mem_value(a, value)
            reg.ppuaddr = a + step
        elif name == 'OAMDATA':
            a = reg.oamaddr
            self.oam[a] = value
            reg.oamaddr = a + 1
        elif name in reg_flag:
            old_v = getattr(reg, name)
            f = reg_flag[name]
            reg_flag[name] = not f
            if f:
                v = (old_v & 0x00ff) | (value << 8)
            else:
                v = (old_v & 0xff00) | value
            setattr(reg, name, v)
        else:
            setattr(reg, name, value)

    @property
    def name_table(self):
        # 现在仅使用名称表 0
        return self.memory[0x2000:0x2400]

    def color_from_palette(self, index):
        c = self.palette[index]
        color = self.colors[c]
        return color

    def pattern_from_table(self, id: int):
        table = self.pattern_table
        number_of_bytes_per_pattern = 16
        beg = id * number_of_bytes_per_pattern
        end = beg + number_of_bytes_per_pattern
        return table[beg:end]

    @property
    def default_color(self):
        i = self.mem_value(0x3f00)
        return self.colors[i]

    def prepare_painter(self, type: str):
        ctrl = self.reg.ppuctrl
        memory = self.memory

        if type == 'background':
            pattern_table_id = int(ctrl & 0b00010000 != 0)
            palette = memory[0x3f00:0x3f10]
        elif type == 'sprites':
            pattern_table_id = int(ctrl & 0b00001000 != 0)
            palette = memory[0x3f10:0x3f20]
        else:
            raise ValueError('错误的画笔类型: <{}>'.format(type))
        beg = pattern_table_id * 0x1000
        end = beg + 0x1000
        pattern_table = memory[beg:end]

        self.pattern_table = pattern_table
        self.palette = palette

    def draw_pattern(
            self, x: int, y: int, pattern_id: int, attribute: int,
            flip_x: bool = False, flip_y: bool = False, covered: bool = False,
    ):
        width, height = self.canvas.get_size()
        canvas = self.canvas
        default_color = self.default_color
        pattern = self.pattern_from_table(pattern_id)
        # 高 2 位来自属性表
        high = attribute

        pattern_size = 8
        for py in range(pattern_size):
            # 边界检测
            yy = y + py
            if yy >= height:
                continue
            # Y 坐标为字节间的偏移
            if flip_y:
                offset = pattern_size - 1 - py
            else:
                offset = py
            # utils.log('y offset: <{}>'.format(offset))
            p0 = pattern[offset]
            p1 = pattern[offset + pattern_size]
            for px in range(pattern_size):
                # 边界检测
                xx = x + px
                if xx >= width:
                    continue
                # 遮挡检测
                pos = xx, yy
                if covered and canvas.get_at(pos) != default_color:
                    continue
                # X 坐标为字节内的偏移
                if flip_x:
                    offset = px
                else:
                    offset = pattern_size - 1 - px
                # 计算低二位
                low0 = (p0 >> offset) & 0b00000001
                low1 = (p1 >> offset) & 0b00000001
                low = (low1 << 1) | low0
                # low 为 0 时，表示「透明」
                if low == 0:
                    continue
                # 合并作为调色盘索引
                i = (high << 2) | low
                # 从调色盘获取颜色
                c = self.color_from_palette(i)
                canvas.set_at(pos, c)

    def draw_background(self):
        self.prepare_painter('background')
        width, height = self.canvas.get_size()
        name_table = self.name_table
        blocks = name_table[:-0x40]
        attr_table = name_table[-0x40:]

        block_size = 8
        blocks_per_line = width // block_size
        for i, item in enumerate(blocks):
            x = (i % blocks_per_line) * block_size
            y = (i // blocks_per_line) * block_size
            pattern_id = item

            # NOTE: 这段有点魔幻，但是能用：）
            # 计算所在属性表
            attr_id = (x >> 5) + (y >> 5) * 8
            attr = attr_table[attr_id]
            # 获取属性表内位偏移
            aoffset = ((x & 0x10) >> 3) | ((y & 0x10) >> 2)
            # 计算高两位
            attr = (attr & (3 << aoffset)) >> aoffset

            self.draw_pattern(x, y, pattern_id, attr)

    def draw_sprites(self):
        self.prepare_painter('sprites')
        oam = self.oam

        bytes_per_sprite = 4
        for i in range(len(oam) // bytes_per_sprite - 1, 0, -1):
            beg = i * bytes_per_sprite
            end = beg + bytes_per_sprite
            # utils.log('beg, end: ', beg, end)
            s = oam[beg:end]

            x = s[3]
            y = s[0] + 1
            flag = s[2]
            pattern_id = s[1]
            attr = flag & 0b00000011
            covered = flag & 0b00100000 != 0
            flip_x = flag & 0b01000000 != 0
            flip_y = flag & 0b10000000 != 0

            self.draw_pattern(x, y, pattern_id, attr, flip_x, flip_y, covered)

    def clear(self):
        canvas = self.canvas
        c = self.default_color
        canvas.fill(c)

    def draw(self):
        # clear
        self.clear()
        # draw
        self.draw_background()
        self.draw_sprites()

    def can_nmi(self):
        v = self.reg.ppuctrl
        mask = 0b10000000
        return (v & mask) != 0

    def enter_vblank(self):
        reg = self.reg
        v = reg.ppustatus
        v |= 0b10000000  # set 「v」 flag
        v ^= 0b01000000  # toggle 「s」 flag
        reg.ppustatus = v
