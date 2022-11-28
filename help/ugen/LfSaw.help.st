"LfSaw ; note Sc2 did not have the initial phase argument"
LfSaw freq: #(500 501) iphase: 1 mul: 0.025

"LfSaw ; used as both oscillator and lfo"
LfSaw freq: (LfSaw freq: 4 iphase: 0 mul: 400 add: #(400 401)) iphase: 0 mul: 0.025

"LfSaw ; output range is bi-polar"
| f |
f := LinLin in: (LfSaw freq: 0.5 iphase: 0) srclo: -1 srchi: 1 dstlo: 200 dsthi: 1600.
(SinOsc freq: {f. 200. 1600} phase: 0 mul: #(0.1 0.05 0.05)) sum

"LfSaw ; as phasor"
(LfSaw freq: 220 iphase: 0 mul: Float pi add: Float pi) sin * 0.1
