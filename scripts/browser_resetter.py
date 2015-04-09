#!/usr/bin/env python2
"""
Resets the browser during each task. 
"""

from __future__ import print_function

from undaemon import Undaemon

import logging.config
import os
import time
import sys
import subprocess as sp
import subprocess
import re
import shlex
import threading
from functools import partial
from logging.handlers import SysLogHandler


DEFAULT_POLL_ENDPOINT           = "https://instr.httpdos.com:1070/startbrowser/"
NOTIFY_UPDATE_COMPLETE_ENDPOINT = "https://instr.httpdos.com:1070/killbrowser/"
NOTIFY_READY                    = "https://instr.httpdos.com:1070/browserready/"
DNSMASQ_CONFIG_PLACE            = "/home/{user}/dnsmasq_more.conf".format(user=os.environ["USER"])
AUX_SSL_PATH                    = "/opt/openssl-1.0.2/"
LD_LIBRARY_PATH                 ="/opt/openssl-1.0.2/lib"
START_TOKEN                     = "KDDFQ"
END_TOKEN                       = "EAJ"
CHROME_CGROUP                   = "/sys/fs/cgroup/chrome"


exec_env = os.environ.copy()
exec_env["PATH"] = AUX_SSL_PATH + "/bin:" +  exec_env["PATH"]
exec_env["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH


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
        'browser_resetter': {
            'handlers':['syslog'],
            'propagate': True,
            'level':'INFO',
        }
    },
}

logging.config.dictConfig(LOGGING)

logger = logging.getLogger("browser_resetter")


station_name = open("/home/ubuntu/Station").read()


def curl_arguments(endpoint, data_binary=""):
    # This function is a bit different because we don't need actual data, 
    # but we do need a status....
    return [
        "curl", 
        "-s", 
        "-o", "/dev/null",
        "-w", "%{http_code}",
        "--data-binary", '{0}'.format(repr(data_binary.encode('ascii'))),
         # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
    ]


def main():
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH
    while True:
        work()


def on_browser_should_finish(undaemon_instance):
    args_get = curl_arguments(
        NOTIFY_UPDATE_COMPLETE_ENDPOINT+station_name, 
        data_binary=END_TOKEN ) # <-- does nothing, really
    while True:
        try:
            print("Executing: ", " ".join(args_get))
            status_code = sp.check_output(args_get)
        except sp.CalledProcessError as e:
            print(" .... Err in curl, returncode: ", e.returncode, file=sys.stderr)
            # Sleep a little bit
            time.sleep(3.0)
        else:
            # Got a token?
            if "200" in status_code :
                undaemon_instance._kill_all()
                print("KILL ALL RETURNED+")
                break
            else:
                print("Bad answer")
                print(os.environ["PATH"])
                time.sleep(3.0)


def work():
    args_get = curl_arguments(
        DEFAULT_POLL_ENDPOINT+station_name, 
        data_binary=START_TOKEN  ) # Also does nothing
    try:
        print("Executing: ", " ".join(args_get))
        status_code = sp.check_output(args_get)
    except sp.CalledProcessError as e:
        print(" .... Err in curl, returncode: ", e.returncode, file=sys.stderr)
        # Sleep a little bit
        time.sleep(3.0)
    else:
        # Got a token?
        if "200" in status_code:
            chrome_process = chrome_run()
            args_ready = curl_arguments(NOTIFY_READY+station_name, data_binary=START_TOKEN)
            # The daemon needs to know when the browser is ready to deliver the url, otherwise
            # the url can be delivered to early ..... 
            sp.check_call(args_ready)
            # Run a thread to watch for the reset
            # signal
            watch = threading.Thread(target = 
                partial(on_browser_should_finish, chrome_process)
            )
            watch.start()

            # And now just wait for the watcher before doing anything...
            watch.join()
        else:
            print("TOKEN IS WRONG")
            print(os.environ["PATH"])
            time.sleep(3.0)


#google-chrome
def tool(cmdstr):
    pieces = shlex.split(cmdstr)
    o = subprocess.check_output(pieces)
    return o 


def run(cmdstr):
    pieces = shlex.split(cmdstr)
    p = subprocess.Popen(pieces)
    return p


def restore_chrome_profile():
    # Most likely you will need to create this directory by hand 
    try:
        subprocess.check_call(shlex.split("rm -rf /home/ubuntu/.config"))
        subprocess.check_call( 
            shlex.split("rsync -avz /home/ubuntu/pristine-config/ /home/ubuntu/.config")
        )
    except subprocess.CalledProcessError:
        print("Didn't work Chrome restore profile")


def chrome_run():
    restore_chrome_profile()
    chrome_process = Undaemon(
        shlex.split("google-chrome --disable-gpu"),
        user=1000,
        undaemon_cgroup_path = CHROME_CGROUP
        )
    undaemon_thread = threading.Thread(
        target= partial(chrome_process.undaemon, set_signal_handlers = False),
         )
    undaemon_thread.start()
    times_tried = 0
    while True:
        time.sleep(1.0)
        s = tool("xwininfo -tree -root")
        mo = re.search(r"\s+(0x[a-f0-9]+) \".*?Google Chrome\"", s)
        if mo is None:
            logger.warning("Couldn't find Google chrome windows,  maybe re-trying ")
            if times_tried > 8:
                logger.error("Exiting chrome script because chrome windows didn't open")
                sys.exit(1) 
            else:
                times_tried += 1
        else:
            logger.info("Found Chrome windows")
            break
    winid = mo.group(1)
    logger.info("Win id: %s", winid)
    tool("xdotool windowsize --sync {0} 100% 100%".format(winid))
    tool("xdotool click --window {0} 1".format(winid))
    time.sleep(1.5)

    # Let's press this key combination a few times to be sure that it works....
    tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid)) # Show
    time.sleep(2.5)
    tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid)) # Hide 
    time.sleep(1.5)
    tool("xdotool key --window {0} \"ctrl+shift+i\"".format(winid)) # Show again
    time.sleep(1.5)

    # Get chrome as full-screen, so to make taking screenshots easier.
    tool("xdotool key --window {0} \"F11\"".format(winid))
    logger.info("Waiting for Chrome process to exit")

    return chrome_process


if __name__ == "__main__":
    main()  