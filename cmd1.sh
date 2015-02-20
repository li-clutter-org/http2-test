#!/bin/bash
#
# This file has special capabilities, establish with:
#sudo setcap 'cap_net_bind_service=+eip' ./cmd1.sh
#
./reh-mimic --action serve --har-file mimic-here/hars/hackage.haskell.org3.har
