#!/usr/bin/env python3

import subprocess
import re
from collections import namedtuple

def shell(command: str) -> str:
	proc = subprocess.run(command, cwd=None, shell=True, capture_output=True)
	if proc.returncode == 0:
		return proc.stdout.decode("utf-8").strip()
	else:
		raise EnvironmentError(
			f"The command [{command}]\nfailed with return code {proc.returncode}.\n"
			f"stderr:\n{proc.stderr.decode('utf-8')}")

Device = namedtuple("Device", ["name", "id", "type"])
Point = namedtuple("Point", ["x", "y"])

def wacom_devices() -> list[Device]:
	raw = shell("xsetwacom list devices")
	for line in raw.split("\n"):
		pattern = re.compile(r"(.*)\s+id: (\d+)\s+type: (\w+)")
		groups = pattern.match(line).groups()
		yield Device(groups[0].strip(), groups[1], groups[2])

devices = {d.type : d.id for d in wacom_devices()}
pad = devices["PAD"]

shell(f"xsetwacom set {pad} Button 1 10")
shell(f"xsetwacom set {pad} Button 2 11")
shell(f"xsetwacom set {pad} Button 3 12")
shell(f"xsetwacom set {pad} Button 8 13")

