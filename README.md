
ReH: A server for HTTP/2 and SPDY
=================================

How SPDY and the upcomming HTTP/2 standard work? This prototype 
implementation of a webserver serving static resources is to learn
a bit about that. It is also a good opportunity to learn how the 
browsers do flow control and how eager are them to interrupt 
resource push from the server.


A simple SPDY ping tool
=======================

Intro
-----

Spdy has a "ping" type of frame. Very handy to establish
network latencies. This simple project does a ping to an
Spdy webserver, and uses the opportunity to gather some 
additional information.

Installation
------------

You need to have Ghc, the Haskell compiler, installed in 
your system. I used the Haskell platform that was current 
at the beginning of 2015. With that done, use the 
following commands:

* After the checkout:

        $ cd spdy-ping-X.X.X/
        $ cabal sandbox init
        $ cabal install
    
That's it!


