"analog bubbles (jmcc) #1"
o := (LFSaw rate: 'kr' freq: #(8 7.23) iphase: 0) * 3 + 80
f := (LFSaw rate: 'kr' freq: 0.4 iphase: 0) * 24 + o
s := (SinOsc rate: 'ar' freq: (f midicps) phase: 0) * 0.04
c := (CombN rate: 'ar' in: s maxdelaytime: 0.2 delaytime: 0.2 decaytime: 4) * 0.1
