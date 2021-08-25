#!/usr/bin/python

import urllib.request
import json
from pathlib import Path
import re

# Example darkskyrc
# key = "00000000000000000000000000000000"
# coord = (00.0000,00.0000)
# units = "us"

CONFIG_PATH = f"{str(Path.home())}/.config/polybar/darkskyrc"


try:
    with open(CONFIG_PATH) as f:
        conf = json.load(f)
        key = conf["key"]
        coord = conf["coord"]
        units = conf["units"]
        url_path = f"{key}/{coord[0]},{coord[1]}?units={units}"
    with urllib.request.urlopen('https://api.darksky.net/forecast/'
                                + url_path) as response:
        raw_text = response.read()
        jdict = json.loads(raw_text)
        with open("/tmp/darksky", "w") as f:
            json.dump(jdict, f)
except urllib.error.URLError:
    print("? ??")
else:
    print(f"{re.sub('-day|-night', '', jdict['currently']['icon'])}"
          f" {round(jdict['currently']['temperature'])}"
          f" %{{T2}}%{{F#7C6F64}}"
          f"{round(jdict['daily']['data'][0]['temperatureMax'])}"
          f"/{round(jdict['daily']['data'][0]['temperatureMin'])}"
          f"%{{T-}}%{{F-}}")
