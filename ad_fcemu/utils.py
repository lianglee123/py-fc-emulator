# -*- coding: utf-8 -*-
import time
import typing as tp


def number_from_bytes(byte_list: tp.List[int], *, signed=False):
    """
    [1]                     => 1
    [0, 1]                  => 256
    [255]                   => 255
    [255], signed=True      => -1
    [255, 255], signed=True => -1
    [256]                   => 因为 256 > 255 所以 raise Exception
    """
    bs = bytes(byte_list)
    return int.from_bytes(bs, 'little', signed=signed)


def log(*args, **kwargs):
    time_format = '%H:%M:%S'
    value = time.localtime(int(time.time()))
    dt = time.strftime(time_format, value)

    print(dt, *args, **kwargs)
    with open('nes.log', 'a', encoding='utf-8') as f:
        print(dt, *args, file=f, **kwargs)
