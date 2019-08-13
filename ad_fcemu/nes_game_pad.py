# -*- coding: utf-8 -*-
class GamePad(object):
    def __init__(self):
        self.value = 0
        self.key_masks = dict(
            a=0b00000001,
            b=0b00000010,
            select=0b00000100,
            start=0b00001000,
            up=0b00010000,
            down=0b00100000,
            left=0b01000000,
            right=0b10000000,
        )
        self.read_counter = 0

    def user_input(self, key: str, pressed: bool):
        # utils.log('user_input: ', key, pressed)
        key = key.lower()
        mask = self.key_masks[key]
        if pressed:
            self.value |= mask
        else:
            self.value &= (~mask)

    def read_for_cpu(self):
        cnt = self.read_counter
        if cnt >= 8:
            cnt -= 8
        self.read_counter = cnt + 1

        mask = 1 << cnt
        f = (self.value & mask) != 0
        return int(f)
