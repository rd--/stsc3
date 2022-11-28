"https://twitter.com/sluyterrific_sc/status/570012853274615808 (es)"
| t o a s |
t := Dust2 density: #(9 9).
o := 0.1.
a := LfNoise2 freq: o mul: 9 add: 9.
s := VarSaw freq: (TExpRand lo: 9 hi: 5000 trig: t) iphase: 0 width: 0.5 mul: (Decay in: t decayTime: 1).
GreyholeRaw in1: (s at: 1) in2: (s at: 2) damping: 0 delaytime: a diffusion: (LfNoise2 freq: 1) feedback: (LfNoise2 freq: 1 mul: 1 add: 1) moddepth: 2 modfreq: a size: o mul: 0.2
