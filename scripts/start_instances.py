# #!env python
"""
Starts all the instances

Check the comment at file "findout_instance_ips.py": the requirements for AWS cli tools 
also apply here.
"""

import json
import subprocess as sp


INSTANCE_NAMES = [
    "testInstance",
    "harvestInstance",
    "serversInstance"
]


def find_instance_ids():
    """
    Returns a dictionary where keys are the following instance names:

        * testInstance
        * harvestInstance
        * serversInstance

    and the values are their public IPS

    :return:
    """
    sym2instance = {}
    for symbolic_name in INSTANCE_NAMES:
        json_description = sp.check_output(
            ["aws",
             "ec2",
             "describe-instances",
             "--filter",
             "Name=tag:Name,Values={0}".format(symbolic_name)
            ])
        # print(json_description)
        obj = json.loads(json_description)
        instance_id = obj["Reservations"][0]["Instances"][0]["InstanceId"]
        sym2instance[symbolic_name] = instance_id
    return sym2instance


def start_instances(sym2instance):
    out = sp.check_output(
        ["aws",
         "ec2",
         "start-instances",
         "--instance-ids",
        ] 
        + 
        list( sym2instance.values() )
    )
    print(out)



if __name__ == "__main__":
    sym2instance = find_instance_ids()
    print(json.dumps(sym2instance))
    start_instances(sym2instance)
