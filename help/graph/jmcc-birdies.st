"birdies (jmcc) #6 ; texture=overlap,7,4,4,inf"
| p1 p2 p3 sw freq amp |
p1 := LFPulse freq: 1.0 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3.0 rand + 4 add: 2.
p2 := LFPulse freq: 1.0 rand + 0.4 iphase: 0 width: 0.8 rand + 0.1 mul: 3.0 rand + 4.
p3 := LFPulse freq: 0.5 rand + 0.2 iphase: 0 width: 0.4 mul: 0.02.
sw := LFSaw freq: (p1 + p2) iphase: 0 mul: 800.0 rand + 1000 negated add: 1200.0 rand2 + 4000.
freq := Lag in: sw lagTime: 0.05.
amp := Lag in: p3 lagTime: 0.3.
Pan2 in: (SinOsc freq: freq kr phase: 0) pos: 1.0 rand2 level: amp kr
