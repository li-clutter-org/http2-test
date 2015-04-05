#!env python
"""
Finds out the current IPs of the stack instances and returns them.....
This script is really just a filter and below uses the AWS cli. To
install such,

    pip install awscli

Be sure to configure AWS cli by following the instructions here:

   http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html#cli-quick-configuration

using the keys provided and the region us-west-2 .

OBS: awscli may run better with python 2, and these tools are generally useful so 
you may want to install them outside the virtualenv and with the System's Python.
"""

import json
import subprocess as sp


INSTANCE_NAMES = [
    "testInstance",
    "harvestInstance",
    "serversInstance"
]


def findout_instance_ips():
    """
    Returns a dictionary where keys are the following instance names:

        * testInstance
        * harvestInstance
        * serversInstance

    and the values are their public IPS

    :return:
    """
    result = {}
    for instance_name in INSTANCE_NAMES:
        json_description = sp.check_output(
            ["aws",
             "ec2",
             "describe-instances",
             "--filter",
             "Name=tag:Name,Values={0}".format(instance_name)
            ])
        obj = json.loads(json_description)
        network_interfaces = obj["Reservations"][0]["Instances"][0]["NetworkInterfaces"]
        for ni in network_interfaces:
            if "Association" in ni:
                result[instance_name] = ni["Association"]["PublicIp"]
                break
    return result


if __name__ == "__main__":
    print(json.dumps(findout_instance_ips()))
