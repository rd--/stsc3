"analog bubbles (jmcc) #1"
| o f s |
o := LFSaw freq: #(8 7.23) iphase: 0 mul: 3 add: 80.
f := LFSaw freq: 0.4 iphase: 0 mul: 24 add: o.
s := SinOsc freq: (f midiCps kr) phase: 0 mul: 0.04.
CombN in: s maxdelaytime: 0.2 delaytime: 0.2 decaytime: 4 mul: 0.1
