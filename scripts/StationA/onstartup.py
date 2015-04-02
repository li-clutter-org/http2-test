#!/usr/bin/python

from __future__ import print_function

import shlex
import subprocess
import re
import sys
import time
import raven
import logging
import logging.config
from logging.handlers import SysLogHandler


LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'verbose': {
            'format': '%(levelname)s %(asctime)s %(module)s %(process)d %(thread)d %(message)s'
        },
        'simple': {
            'format': '%(levelname)s %(message)s'
        },
    },
    'handlers': {
        'syslog':{
            'level':'DEBUG',
            'class':'logging.handlers.SysLogHandler',
            'formatter': 'simple',
            'facility': SysLogHandler.LOG_LOCAL2,
        }
    },
    'loggers': {
        'onstartup': {
            'handlers':['syslog'],
            'propagate': True,
            'level':'INFO',
        }
    },
}
logging.config.dictConfig(LOGGING)

logger = logging.getLogger("onstartup")

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
while True:
	time.sleep(4.0)
	s = tool("xwininfo -tree -root")
	mo = re.search(r"\s+(0x[a-f0-9]+) \".*?Google Chrome\"", s)
	if mo is None:
		logger.warning("Couldn't find Google chrome windows,  maybe re-trying ")
		if chrome_process.returncode is not None:
			logger.error("Chrome exited with code %d", chrome_process.returncode)
			exit(1) 
	else:
		break
winid = mo.group(1)
logger.info("Win id: %s", winid)
tool("xdotool windowsize --sync {0} 100% 100%".format(winid))
tool("xdotool click --window {0} 1".format(winid))
time.sleep(0.5)

# Let's press this key combination a few times to be sure that it works....
tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid))
time.sleep(3.5)
tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid))
time.sleep(3.5)
tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid))
time.sleep(3.5)

# Get chrome as full-screen, so to make taking screenshots easier.
tool("xdotool key --window {0} \"F11\"".format(winid))
logger.info("Waiting for Chrome process to exit")
chrome_process.wait()
