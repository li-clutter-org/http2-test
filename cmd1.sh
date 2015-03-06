#!/bin/bash
#
# This file has special capabilities, establish with:
#sudo setcap 'cap_net_bind_service=+eip' ./cmd1.sh
#
source makeenv
./reh-mimic --action=research --har-file=irrelevant
