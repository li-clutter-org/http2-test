#!env python
"""
Freeze the browser-bearing instances (the ones that we denote as StationA and 
StationB). 

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


def stop_instances(sym2instance):
    out = sp.check_output(
        ["aws",
         "ec2",
         "stop-instances",
         "--instance-ids",
        ] 
        + 
        list( sym2instance.values() )
    )
    print(out)


def create_image(sym2instance, suffix):
    for (symbolic_name, instance_id) in sym2instance.items():
        out = sp.check_output(
            ["aws",
             "ec2",
             "create-image",
             "--instance-id",
             instance_id,
             "--name", symbolic_name + "_" + suffix,
             "--description", symbolic_name
            ] 
        )
        print(out)    


if __name__ == "__main__":
    sym2instance = find_instance_ids()
    print(json.dumps(sym2instance))
    stop_instances(sym2instance)
    create_image(sym2instance, "1")
