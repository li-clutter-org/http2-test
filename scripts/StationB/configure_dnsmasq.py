#
#  Alcides Viamontes Esquivel
#  Zunzun AB
#  www.zunzun.se
#
#  Copyright Load Impact AB, 2015
#

"""
This script should be ran with elevated privileges, so that it can restart dnsmasq
"""

from __future__ import print_function
import shlex
import time
import os
import os.path
import subprocess as sp


import daemon




# It may be a good idea to have some flexibility here, later see to that.... (TODO)
# import argparse


DEFAULT_POLL_ENDPOINT = "https://instr.httpdos.com:1070/dnsmasq/"
NOTIFY_UPDATE_COMPLETE_ENDPOINT = "https://instr.httpdos.com:1070/dnsmasqupdated/"
DNSMASQ_CONFIG_PLACE = "/home/{user}/dnsmasq_more.conf".format(user=os.environ["USER"])
AUX_SSL_PATH= "/opt/openssl-1.0.2/"

def curl_arguments(endpoint):
    return [
        "curl", "-k", "--data-binary", '""', # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
    ]


def main():
    
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    while True:
        work()


def work():
    args_get = curl_arguments(DEFAULT_POLL_ENDPOINT)
    args_update_completed = curl_arguments(NOTIFY_UPDATE_COMPLETE_ENDPOINT)
    try:
        config_file = sp.check_output(args_get)
    except sp.CalledProcessError as e:
        print(" .... Err in curl, returncode: ", e.returncode)
        # Sleep a little bit
        time.sleep(3.0)
    else:
        # Got a config_file?
        if len(config_file) > 5:
            # Seems like so.... pass on to
            with open(DNSMASQ_CONFIG_PLACE, "w") as out:
                out.write(config_file)
            print(".newconfig written to file")
            # Now restart dnsmasq... here is why I need elevated privileges....
            sp.check_call(shlex.split("/etc/init.d/dnsmasq restart"))
            # And issue a new call to the Haskell server to let it know 
            # that the new dnsmasq setting has taken effect


if __name__ == "__main__":
    # with daemon.DaemonContext():
    main()  



