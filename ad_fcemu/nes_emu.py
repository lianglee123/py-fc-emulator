# -*- coding: utf-8 -*-
import typing as tp

import pygame as pg

import config
import nes_cpu as nc
import nes_file as nf
import nes_game_pad as ngp
import nes_ppu as np


class Emulator(object):
    def __init__(self):
        self.cpu: nc.NesCPU = None
        self.memory: nc.Memory = None
        self.ppu: np.NesPPU = None
        self.canvas: pg.Surface = None
        self.colors: tp.List[tp.Tuple[int, int, int, int]] = None
        self.pad: ngp.GamePad = None
        self.key_mapper: tp.Dict[int, str] = None

        self.setup()

    def setup(self):
        width, height = 256, 240
        canvas = pg.display.set_mode((width, height))
        ppu = np.NesPPU(canvas)
        pad = ngp.GamePad()
        mem = nc.Memory(ppu, pad)
        cpu = nc.NesCPU(mem)

        self.cpu = cpu
        self.memory = mem
        self.ppu = ppu
        self.pad = pad
        self.key_mapper = {
            pg.K_j: 'a',
            pg.K_k: 'b',
            pg.K_u: 'select',
            pg.K_i: 'start',
            pg.K_w: 'up',
            pg.K_s: 'down',
            pg.K_a: 'left',
            pg.K_d: 'right',
        }

    def load(self, path: str):
        mem, ppu = self.memory, self.ppu
        nes = nf.NesFile.load(path)
        mem.load_nes(nes)
        ppu.load_nes(nes)

    def run(self):
        cpu, ppu, pad, key_mapper = self.cpu, self.ppu, self.pad, self.key_mapper
        clock = pg.time.Clock()
        running = True
        fps = 30

        cpu.interrupt('RESET')
        pg.display.set_caption('NES')
        while running:
            for event in pg.event.get():
                if event.type == pg.QUIT:
                    running = False
                elif event.type == pg.KEYDOWN:
                    key = event.key
                    if key in key_mapper:
                        k = key_mapper[key]
                        pad.user_input(k, True)
                    elif key == pg.K_g:
                        config.DEBUG = not config.DEBUG
                elif event.type == pg.KEYUP:
                    key = event.key
                    if key in key_mapper:
                        k = key_mapper[key]
                        pad.user_input(k, False)

            ppu.enter_vblank()
            if ppu.can_nmi():
                cpu.interrupt('NMI')
            for _ in range(5000):
                cpu.execute()

            ppu.draw()
            pg.display.flip()
            clock.tick(fps)
