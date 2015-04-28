
from __future__ import print_function 

import json 
import textwrap


H1 = "harvested.har"
H2 = "test_http2.har"

H1_json = json.load(open(H1))
H2_json = json.load(open(H2))


def lens(*keys):
    "An expat from Haskell (but it is a poor cousin)"
    def f(obj, new_value=None):
        if new_value is None:
            # Act as a getter
            x = obj 
            for key in keys:
                x = x[key]
            return x
        else:
            # Act as a setter
            x = obj 
            xp = None
            for key in keys:
                xp = x 
                x = x[key]
            xp[key] = new_value 
            return None
    return f

def print_paragraph(paragraph):
    lines = textwrap.wrap(paragraph, width=80)
    for line in lines:
        print(line)

print_paragraph("""
First question, do resources are loaded in the same order
from both files?
""")

entries_lens = lens('har', 'entries')
entry2url = lens('request', 'url')

for (i, e1, e2) in zip( range(10), entries_lens(H1_json), entries_lens(H2_json)):
    print("*")
    print("    http1: ", entry2url(e1) )
    print("    http2: ", entry2url(e2) )

print_paragraph("""
Answer: no. That make things harder, I didn't realize before how much.... """)

print_paragraph(""" 
What about when thequests start, and how different are their times?
""")

entry2requestStarted = lens('startedDateTime' )
for (i, e1) in zip( range(10), entries_lens(H1_json)):
    print("*")
    print("    http1-request: ", entry2requestStarted(e1) )


print_paragraph("""
What is the unit of measure of the durations?""")
wait_lens = lens("timings", "wait")
for (i, e1) in zip( range(10), entries_lens(H1_json) ):
    print("*")
    wait_time = wait_lens(e1)
    print("    wait-time: ", wait_time, type(wait_time) )
    
print_paragraph("""Looks like milliseconds""")

#enterlens = lens("response", "content", "text")
#for (i, e1) in zip( range(10), entries_lens(H2_json)):
    #print("*")
    #txt = enterlens(e1)
    #print("    response_keys: ", type(txt),  txt)