#!/usr/bin/python

import json
import os
import locale
from datetime import datetime
from subprocess import Popen, PIPE

ENV = os.environ.copy()
ENV['LC_ALL'] = 'C'
ENC = locale.getpreferredencoding()
ROFI_CMD = [
    "rofi",
    "-location", "3",
    "-dmenu", "-i",
    "-theme-str", "#inputbar {enabled:false;}",
    "-xoffset", "-15",
    "-width", "425",
    "-markup-rows"
]


def run_rofi(args, lines):
    return (Popen(args, stdin=PIPE, stdout=PIPE, env=ENV)
            .communicate(input="\n".join(lines).encode(ENC))[0]
            .decode(ENC)).strip()


def main():
    with open("/tmp/darksky") as f:
        jdict = json.load(f)
    run_rofi(ROFI_CMD,
             ["<span color='#665C54'>"
              f"{datetime.fromtimestamp(i['time']).strftime('%a')}"
              "</span>"
              f"  {round(i['temperatureMin'])}°"
              f" - {round(i['temperatureMax'])}° {i['icon']}"
              for i in jdict['daily']['data']])


if __name__ == '__main__':
    main()
