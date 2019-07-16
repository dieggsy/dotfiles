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
    "-width", "300",
    "-markup-rows"
]


def get_icon(icon):
    switcher = {
        "clear-day": "",
        "clear-night": "",
        "rain": "",
        "snow": "",
        "sleet": "",
        "wind": "",
        "fog": "",
        "cloudy": "",
        "partly-cloudy-day": "",
        "partly-cloudy-night": "",
        "hail": "",
        "thunderstorm": "",
        "tornado": "",
    }
    return switcher.get(icon, "?")


def run_rofi(args, lines):
    return (Popen(args, stdin=PIPE, stdout=PIPE, env=ENV)
            .communicate(input="\n".join(lines).encode(ENC))[0]
            .decode(ENC)).strip()


def main():
    with open("/tmp/darksky") as f:
        jdict = json.load(f)
    run_rofi(ROFI_CMD,
             ["<span color='#665C54'>"
              f"{datetime.fromtimestamp(i['time']).strftime('%H:%M')}"
              "</span>"
              f"  {round(i['temperature'])}°  {get_icon(i['icon'])}"
              for i in jdict['hourly']['data']][:24])


if __name__ == '__main__':
    main()
