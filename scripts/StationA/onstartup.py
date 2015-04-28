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
            'format': 'ONSTARTUP %(levelname)s %(message)s'
        },
    },
    'handlers': {
        'syslog':{
            'level':'DEBUG',
            'class':'logging.handlers.SysLogHandler',
            'formatter': 'simple',
            'facility': SysLogHandler.LOG_LOCAL2,
            'address': '/dev/log'
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
        status_code = 'none'
        try:
            p  = sp.Popen([
                    "/home/ubuntu/browser_resetter/browser_resetter"
                    ])
            status_code = p.wait()
        except Exception as x:
            logger.exception("When executing browser resetter")
        # Report the code
        if status_code != 0:
            logger.error("Resetter exited with code %d",  status_code)
        time.sleep(1)
        # And go for it again


if __name__ == '__main__':
    main()
