#!/usr/bin/env python2
"""
Resets the browser during each task. 
"""

from .undaemon import Undaemon

DEFAULT_POLL_ENDPOINT           = "https://instr.httpdos.com:1070/browser/"
NOTIFY_UPDATE_COMPLETE_ENDPOINT = "https://instr.httpdos.com:1070/browserupdated/"
DNSMASQ_CONFIG_PLACE            = "/home/{user}/dnsmasq_more.conf".format(user=os.environ["USER"])
AUX_SSL_PATH                    = "/opt/openssl-1.0.2/"
LD_LIBRARY_PATH                 ="/opt/openssl-1.0.2/lib"
START_TOKEN                     = "KDDFQ"
END_TOKEN                       = "EAJ"
CHROME_CGROUP                   = "/sys/fs/cgroup/chrome"


def curl_arguments(endpoint, data_binary=""):
    return [
        "curl", "-sS", "-k", "--data-binary", '{0}'.format(repr(data_binary.encode('ascii'))), # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
    ]


def main():
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    os.environ["LD_LIBRARY_PATH"] = LD_LIBRARY_PATH
    while True:
        work()


def on_browser_should_finish(undaemon_instance):
    args_get = curl_arguments(
        NOTIFY_UPDATE_COMPLETE_ENDPOINT, 
        data_binary=MY_VERSION)
    while True:
        try:
            print("Executing: ", " ".join(args_get))
            token = sp.check_output(args_get)
        except sp.CalledProcessError as e:
            print(" .... Err in curl, returncode: ", e.returncode, file=sys.stderr)
            # Sleep a little bit
            time.sleep(3.0)
        else:
            # Got a token?
            if token == END_TOKEN
                undaemon._kill_all()
                break;
            else:
                print("TOKEN IS WRONG")
                print(os.environ["PATH"])
                time.sleep(3.0)


def work():
    args_get = curl_arguments(DEFAULT_POLL_ENDPOINT, data_binary=MY_VERSION)
    try:
        print("Executing: ", " ".join(args_get))
        token = sp.check_output(args_get)
    except sp.CalledProcessError as e:
        print(" .... Err in curl, returncode: ", e.returncode, file=sys.stderr)
        # Sleep a little bit
        time.sleep(3.0)
    else:
        # Got a token?
        if token == START_TOKEN:
            chrome_process = chrome_run()
            # Run a thread to watch for the reset
            # signal
            watch = threading.Thread(target = 
                partial(on_browser_should_finish, chrome_process)
            )

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


def chrome_run():
    chrome_process = Undaemon(
        shlex.split("google-chrome --disable-gpu"),
        user=1000,
        undaemon_cgroup_path = CHROME_CGROUP
        )
    undaemon_thread = threading.Thread(
        target=chrome_process.undaemon )
    undaemon_thread.start()
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
    return chrome_process





if __name__ == "__main__":
    main()  