"sprinkler mouse (jmcc) #1"
|f t o|
f := MouseX minval: 0.2 maxval: 50 warp: 0 lag: 0.2 .
t := LFPulse freq: f iphase: 0 width: 0.25 mul: 0.1 .
o := BPZ2 in: (WhiteNoise mul: t kr) .
o
