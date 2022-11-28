"sprinkler (jmcc) #1"
| f t |
f := LfPulse freq: 0.09 iphase: 0 width: 0.16 mul: 10 add: 7.
t := LfPulse freq: f iphase: 0 width: 0.25 mul: 0.1.
Bpz2 in: (WhiteNoise mul: t kr)
