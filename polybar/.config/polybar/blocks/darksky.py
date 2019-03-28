#!/usr/bin/python

import urllib.request
import json
from pathlib import Path

# Example darkskyrc
# key = "00000000000000000000000000000000"
# coord = (00.0000,00.0000)
# units = "us"

config = f"{str(Path.home())}/.config/polybar/darkskyrc"
with open(config) as f:
    exec(f.read())

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
    return switcher.get(icon, "")

with urllib.request.urlopen('https://api.darksky.net/forecast/'
                            f'{key}/{coord[0]},{coord[1]}?units={units}') as response:
    json = json.loads(response.read())

print(f"{get_icon(json['currently']['icon'])} {round(json['currently']['temperature'])}")
