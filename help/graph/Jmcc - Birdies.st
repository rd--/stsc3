"birdies (jmcc) #6 ; texture=overlap,7,4,4,inf"
| p1 p2 p3 sw freq amp |
p1 := LfPulse freq: 1 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3 rand + 4 add: 2.
p2 := LfPulse freq: 1 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3 rand + 4.
p3 := LfPulse freq: 0.5 rand + 0.2 iphase: 0 width: 0.4 mul: 0.02.
sw := LfSaw freq: (p1 + p2) iphase: 0 mul: 800 rand + 1000 negated add: 1200 rand2 + 4000.
freq := Lag in: sw lagTime: 0.05.
amp := Lag in: p3 lagTime: 0.3.
Pan2 in: (SinOsc freq: freq kr phase: 0) pos: 1 rand2 level: amp kr
