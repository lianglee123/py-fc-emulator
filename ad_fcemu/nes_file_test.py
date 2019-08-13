# -*- coding: utf-8 -*-
import nes_file as nf


def prepared_nes():
    return nf.NesFile.load('misc/nestest.nes')


def test_load1():
    nes = prepared_nes()
    expected = 'NES'
    result = nes.format
    assert expected == result, result


def test_load2():
    nes = prepared_nes()
    expected = 16384 * nes.prg_rom_unit_size
    result = len(nes.prg_rom)
    assert expected == result, result


def test_load3():
    nes = prepared_nes()
    expected = 8192 * nes.chr_rom_unit_size
    result = len(nes.chr_rom)
    assert expected == result, result


def test_bytes_to_int_list():
    test_cases = [
        (b'', []),
        (b'\x00', [0]),
        (b'\x00\x01', [0, 1]),
        (b'\xff', [255]),
    ]
    for case in test_cases:
        result = list(case[0])
        expected = case[1]
        assert expected == result, (case, result)
