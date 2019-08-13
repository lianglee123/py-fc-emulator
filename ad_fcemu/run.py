# -*- coding: utf-8 -*-
import nes_emu as ne


def main():
    emu = ne.Emulator()
    emu.load('misc/balloon.nes')
    emu.run()


if __name__ == '__main__':
    main()
