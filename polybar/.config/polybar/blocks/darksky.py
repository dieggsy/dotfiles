#!/usr/bin/python

import urllib.request
import json
from pathlib import Path

# Example darkskyrc
# key = "00000000000000000000000000000000"
# coord = (00.0000,00.0000)
# units = "us"

CONFIG_PATH = f"{str(Path.home())}/.config/polybar/darkskyrc"

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

try:
    with open(CONFIG_PATH) as f:
        exec(f.read())
    with urllib.request.urlopen('https://api.darksky.net/forecast/'
                                f'{key}/{coord[0]},{coord[1]}?units={units}') as response:
        raw_text = response.read()
        jdict = json.loads(raw_text)
        with open("/tmp/darksky", "w") as f:
            json.dump(jdict,f);
except:
    print("? ??")
else:
    print(f"{get_icon(jdict['currently']['icon'])} {round(jdict['currently']['temperature'])}"
          f" %{{T4}}%{{F#665C54}}{round(jdict['daily']['data'][0]['temperatureMax'])}"
          f"/{round(jdict['daily']['data'][0]['temperatureMin'])}%{{T-}}%{{F-}}")

