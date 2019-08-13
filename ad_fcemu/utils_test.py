# -*- coding: utf-8 -*-
import utils


def test_number_from_bytes():
    test_cases = [
        (dict(byte_list=[1]), 1),
        (dict(byte_list=[1, 0]), 1),
        (dict(byte_list=[0, 1]), 256),
        (dict(byte_list=[255]), 255),
        (dict(byte_list=[255], signed=True), -1),
        (dict(byte_list=[255, 255], signed=True), -1),
        (dict(byte_list=[0, 255], signed=True), -256),
    ]
    for case in test_cases:
        input = case[0]
        expected = case[1]
        result = utils.number_from_bytes(**input)
        assert expected == result, (case, result)
