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
import signal
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
# Time in seconds to wait
#KILL_BROWSER_AFTER              = 40
# Debug time
KILL_BROWSER_AFTER              = 50
TIMES_TO_CHECK_FOR_CHROME       = 50


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
            'format': 'RESETTER %(levelname)s %(message)s'
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
        'browser_resetter': {
            'handlers':['syslog'],
            'propagate': True,
            'level':'INFO',
        },
        'undaemon': {
            'handlers':['syslog'],
            'propagate': True,
            'level':'INFO',
        }
    },
}
logging.config.dictConfig(LOGGING)
logger = logging.getLogger("browser_resetter")
logger.info("STARTING BROWSER RESETTER")


station_name = open("/home/ubuntu/Station").read()


def curl_arguments(endpoint, data_binary="", use_output=False):
    # This function is a bit different because we don't need actual data, 
    # but we do need a status....
    return [
        "curl", 
        "-s",  ] + \
        ([ "-o", "/dev/null" ] if not use_output else []) + \
        ["-w", "%{http_code}",
        "--data-binary", '{0}'.format(repr(data_binary.encode('ascii'))),
         # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
        ]


def main():
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH
    set_signal_handlers()
    while True:
        work()
        
        
current_kill_watch = None


def on_usr1(arg1, arg2):
    logger.info("Received SIGUSR1")
    if current_kill_watch is not None:
        current_kill_watch(arg1, arg2)

        
def set_signal_handlers():
    signal.signal(signal.SIGUSR1, on_usr1)
    #signal.signal(signal.SIGALRM, self._alarm_handler)
    #signal.signal(signal.SIGTERM, self._kill_all)    


class BrowserKillWatch(object):
    
    def __init__( self, undaemon_instance, hash_id):
        self._undaemon_instance = undaemon_instance
        self._hashid = hash_id 
        self._on_exit_lock = threading.Lock()
        self._browser_already_killed = threading.Event()
        self._browser_on_normal_exit = threading.Event()
        global current_kill_watch
        current_kill_watch = self.on_usr1
        
    def run(self):
        self._kill_timer = threading.Timer(KILL_BROWSER_AFTER, self.timed_kill)
        self._kill_timer.start()
        self.on_browser_should_finish()
        
    def on_usr1(self, arg1, arg2):
        if self._browser_on_normal_exit.is_set() :
            logger.info("Received SIGUSR1 on normal exit")
        else:
            logger.info("Received unexpected SIGUSR1")
            # Force the remaining finalization mechanisms to acknowledge that the browser exited,
            # so that we can start over.
            self._browser_already_killed.set()
        
    def __call__(self):
        self.run()

    def on_browser_should_finish(self):
        undaemon_instance, hashid = self._undaemon_instance, self._hashid
        args_get = [
            "curl", 
            "-s",  # Silent mode
            "-w", "status=%{http_code}",
            "-m", str(KILL_BROWSER_AFTER), # Establish a timeout
            "--data-binary", END_TOKEN.encode('ascii'),
             # I don't think any data needs to be submitted
             "-X", "POST", "--http2", NOTIFY_UPDATE_COMPLETE_ENDPOINT+station_name
             ]        
        while True:
            if self._browser_already_killed.is_set():
                # Well, well met
                break
            try:
                logger.info("Executing: %s", " ".join(args_get))
                process_output = sp.check_output(args_get)
            except sp.CalledProcessError as e:
                logger.error(" .... Err in curl, returncode: %d ", e.returncode)
                # Sleep a little bit
                time.sleep(3.0)
            else:
                status_code, returned_hash_id = token_and_status_from_curl_output(process_output)
                logger.info("For killing, just obtained status_code %s and hash  %s", status_code, returned_hash_id)
                # Got a token?
                if status_code=="200":
                    if returned_hash_id == hashid :
                        self.log_and_kill_the_browser()
                        break
                    else:
                        logger.warning("Expected hashid=%s and received hashid was %s, will keep waiting",
                                     hashid, returned_hash_id )
                        # self.log_and_kill_the_browser()
                        break
                else:
                    logger.error("When-to-kill returned non 200 status code: %s", status_code )
                    print(os.environ["PATH"])
                    time.sleep(3.0)
                    
    def timed_kill(self):
        if not self._browser_already_killed.is_set():
            logger.info("Attemping timed kill of the browser")
            self.log_and_kill_the_browser()

    def log_and_kill_the_browser(self):
        if not self._browser_already_killed.is_set():
            self._browser_on_normal_exit.set()
            with self._on_exit_lock:
                undaemon_instance = self._undaemon_instance
                logger.info("Going to kill the Chrome process and all its descendance, if this is the last message you see, we have a problem")
                (terminated, killed) = undaemon_instance.kill_all()
                logger.info("Terminated: %d, Killed: %d", terminated, killed)
                self._browser_already_killed.set()


def work():
    args_get = [
        "curl", 
        "-s",  # Silent mode
        "-w", "status=%{http_code}",
        "--data-binary", START_TOKEN.encode('ascii'),
         # I don't think any data needs to be submitted
         "-X", "POST", "--http2", DEFAULT_POLL_ENDPOINT+station_name
         ]    
    try:
        logger.info("Executing: %s", " ".join(args_get))
        process_output = sp.check_output(args_get)
    except sp.CalledProcessError as e:
        logger.error(" .... Err in curl, returncode: %d ", e.returncode)
        # Sleep a little bit
        time.sleep(3.0)
    else:
        status_code, hashid = token_and_status_from_curl_output(process_output)
        logger.info("Start browser, status_code=%s, hashid=%s", status_code, hashid)
        if "200" in status_code:
            chrome_process = chrome_run()
            args_ready = curl_arguments(NOTIFY_READY+station_name, data_binary=START_TOKEN)
            # The daemon needs to know when the browser is ready to deliver the url, otherwise
            # the url can be delivered too early ..... 
            sp.check_call(args_ready)
            # Run a thread to watch for the reset
            # signal
            kill_watch = BrowserKillWatch(chrome_process, hashid)
            watch = threading.Thread(target = kill_watch)
            watch.start()

            # And now just wait for the watcher before doing anything...
            watch.join()
        else:
            logger.error("Invalid status code in HTTP response: %s", status_code )
            time.sleep(3.0)


def token_and_status_from_curl_output(process_output):
    # Got a good result code?
    mo = re.search(r"status=(\d+)", process_output )
    status_code = mo.group(1)
    # And maybe a token?
    mo = re.search(r"hashid=([A-Za-z0-9]{3,})", process_output)
    hashid = mo and mo.group(1)
    return status_code, hashid


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
        cmd_out = open("/dev/null", "a")
        subprocess.check_call( 
            shlex.split("rsync -avz /home/ubuntu/pristine-config/ /home/ubuntu/.config"),
            stdout = cmd_out
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
        time.sleep(1.4)
        s = tool("xwininfo -tree -root")
        mo = re.search(r"\s+(0x[a-f0-9]+) \".*?Google Chrome\"", s)
        if mo is None:
            logger.warning("Couldn't find Google chrome windows,  maybe re-trying ")
            if times_tried > TIMES_TO_CHECK_FOR_CHROME:
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