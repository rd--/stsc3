"analog bubbles (jmcc) #1"
o := (LFSaw freq: #(8 7.23) iphase: 0) * 3 + 80 .
f := (LFSaw freq: 0.4 iphase: 0) * 24 + o .
s := (SinOsc freq: (f midicps kr) phase: 0) * 0.04 .
(CombN in: s maxdelaytime: 0.2 delaytime: 0.2 decaytime: 4) * 0.1
