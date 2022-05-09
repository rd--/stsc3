"LFSaw ; note SC2 did not have the initial phase argument"
LFSaw freq: #(500 501) iphase: 1 mul: 0.025

"LFSaw ; used as both oscillator and lfo"
LFSaw freq: (LFSaw freq: 4 iphase: 0 mul: 400 add: #(400 401)) iphase: 0 mul: 0.025

"LFSaw ; output range is bi-polar"
| f |
f := LinLin in: (LFSaw freq: 0.5 iphase: 0) srclo: -1 srchi: 1 dstlo: 200 dsthi: 1600.
(SinOsc freq: {f. 200. 1600} phase: 0 mul: #(0.1 0.05 0.05)) sum
