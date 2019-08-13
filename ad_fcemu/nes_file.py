# -*- coding: utf-8 -*-


class NesFile(object):
    def __init__(self, data: bytes):
        self.format = None
        self.prg_rom_unit_size = None
        self.chr_rom_unit_size = None
        self.trainer = None
        self.prg_rom = None
        self.chr_rom = None

        self._setup(data)

    def _setup(self, data: bytes):
        format = data[0:3].decode('ascii')  # 舍弃最后的 1A
        prg_rom_unit_size = data[4]
        chr_rom_unit_size = data[5]
        flag6 = data[6]
        has_trainer = flag6 & 0b0000100 != 0

        if has_trainer:
            t_beg = 16
            t_end = t_beg + 512
            trainer = list(data[t_beg:t_end])
            prom_beg = t_end
        else:
            trainer = None
            prom_beg = 16
        prom_len = prg_rom_unit_size * 16384
        prom_end = prom_beg + prom_len
        prg_rom = list(data[prom_beg:prom_end])

        crom_len = chr_rom_unit_size * 8192
        crom_beg = prom_end
        crom_end = crom_beg + crom_len
        chr_rom = list(data[crom_beg:crom_end])

        self.format = format
        self.prg_rom_unit_size = prg_rom_unit_size
        self.chr_rom_unit_size = chr_rom_unit_size
        self.trainer = trainer
        self.prg_rom = prg_rom
        self.chr_rom = chr_rom

    @classmethod
    def load(cls, path):
        with open(path, 'rb') as f:
            data = f.read()
            return cls(data)
