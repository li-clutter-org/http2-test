#!/bin/bash

scp dist/build/reh-mimic/reh-mimic http2study@192.168.56.101:/usr/local/bin/
rsync -avz mimic http2study@192.168.56.101:/home/http2study/
