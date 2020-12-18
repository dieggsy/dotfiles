#!/usr/bin/python

import sys, unicodedata

# Print all named unicode chars
try:
    # Max of range chosen experimentally, lol
    for i in range(32,918000):
        try:
            char = chr(i)
            print(f'U+{i:05x}\t{char}\t{unicodedata.name(char)}')
        except ValueError:
            continue
    # try:
    #     print(f'{i}\t{char}')
    # except UnicodeEncodeError:
    #     print(i)
except (BrokenPipeError, IOError):
    pass

sys.stderr.close()
