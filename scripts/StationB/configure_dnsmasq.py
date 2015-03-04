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

# It may be a good idea to have some flexibility here, later see to that.... (TODO)
# import argparse


DEFAULT_POLL_ENDPOINT = "https://instr.httpdos.com:1070/dnsmasq/"
DNSMASQ_CONFIG_PLACE = "/home/http2study/dnsmasq_more.conf"
AUX_SSL_PATH= "/opt/openssl-1.0.2/"

def curl_arguments(endpoint):
    return [
        "curl", "-k", "--data-binary", '""', # I don't think any data needs to be submitted
        "-X", "POST", "--http2", endpoint
    ]


def main():
    args = curl_arguments(DEFAULT_POLL_ENDPOINT)
    os.environ["PATH"] = os.path.join(AUX_SSL_PATH, "bin/") + ":" + os.environ["PATH"]
    while True:
        work(args)

def work(args):
    try:
        config_file = sp.check_output(args)
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


if __name__ == "__main__":
    main()

