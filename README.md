
Introduction: Instrumenting HTTP/2 with ReH
===========================================

ReH can reproduce the load sequence of a website. To do that, 
it first asks a browser (Google Chrome as of the time of this 
writing) to capture all the requests and responses that make
a given website, and then serves back the website. Read more in 
one of the sections below, "Endpoints". 

## The parts 

There are three big components in the instrumentation server, and they
match independent virtual machines. Let's 
use names for them: StationA, StationB and Instr. The Instr component 
should run the ReH, while both StationA and StationB should run Google  Chrome
with an extension installed. The extension can be found in a subrepository here. 


Installation
============

## The short version

* After the checkout and while being at the project directory, install the instrumentation 
  webserver in the following way:

        $ cabal sandbox init
        $ cabal install

* [TODO: Write a more detailed instance-setup procedure] Scripts and extensions to be distributed to the station machines:

        $ fab apt-stations
    
That's it!

## The long version 

The ReH server is written in Haskell, which is a very decent functional and
imperative language. There is a package manager for Haskell with also very decent
dependency management, which is called Cabal. This project can be installed 
with Cabal. 

Endpoints
=========

ReH opens a control webserver at port 1070. This is an HTTP/2 service running
with a configured certificate. On that webserver, ReH listens for requests from 
both external applications (e.g, the Django web server) and from the two machines
running Chrome instances. 

## Public endpoints

* */setnexturl/* POST to this uri the next URL to analyze (as the POST data). After this,
  ReH starts the analysis of the given URL. The result of this stage is some .har files 
  produced by loading the website under different network conditions and protocols ( *TODO:
  explain which protocols and network conditions, and where the files are created* ).



Configuration
=============

_to be written_
