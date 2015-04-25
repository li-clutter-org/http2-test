
Snippets
========


Common actions
--------------

This file contains commands that I use during development of this project.


To create a research server:

    $ ./reh-mimic --action=research --har-file=irrelevant

Redeploy the extension at StationA:

    $ fab StationA

How to queue a url:

    $ curl -k --data-binary "http://www.yahoo.com/" -X POST --http2 https://instr.httpdos.com:1070/setnexturl/

Curl version 7.40 or greater is needed.


Deployment actions
------------------

Installing the latest version of the executable:

    $ fab BeefyRehMimic

Initializing the Chrome extension
---------------------------------

    _myapi.setEndPoints("StationA")
    _myapi.setEndPoints("StationB")
