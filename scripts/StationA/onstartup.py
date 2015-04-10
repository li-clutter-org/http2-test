#!/usr/bin/python

from __future__ import print_function

import shlex
import subprocess as sp
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

def main():
    logger.info("onstartup, invoking browser resetter")
    # sp.check_call(["xterm"])
    while True:
        p  = sp.Popen([
                "/home/ubuntu/browser_resetter/browser_resetter"
                ])
        p.wait()
        # Sleep a bit 
        time.sleep(1000)
        # And go for it again


if __name__ == '__main__':
    main()
