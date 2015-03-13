#!/usr/bin/python

from __future__ import print_function

import shlex
import subprocess
import re
import sys
import time

#google-chrome
def tool(cmdstr):
    pieces = shlex.split(cmdstr)
    o = subprocess.check_output(pieces)
    return o 

def run(cmdstr):
    pieces = shlex.split(cmdstr)
    p = subprocess.Popen(pieces)
    return p

chrome_process = run("google-chrome --disable-gpu")
time.sleep(5.0)
s = tool("xwininfo -tree -root")
mo = re.search(r"\s+(0x[a-f0-9]+) \".*?Google Chrome\"", s)
if mo is None:
	print("Couldn't find Chrome windows")
else:
	winid = mo.group(1)
	print("Win id:", winid)
	tool("xdotool windowsize --sync {0} 100% 100%".format(winid))
	tool("xdotool click --window {0} 1".format(winid))
	time.sleep(0.5)
	tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid))
	time.sleep(0.5)
	# Get chrome as full-screen, so to make taking screenshots easier.
	tool("xdotool key --window {0} \"F11\"".format(winid))
	chrome_process.wait()
