"f0 ; https://twitter.com/redFrik/status/839296011982024704"
| t f m i g |
t := LFSaw freq: #(0.5 0.6) iphase: 0.
f := (LFSaw freq: 5 iphase: 0 mul: (LFSaw freq: 0.015 iphase: 0) add: 1) * 98.
m := (2 ** ((LFSaw freq: 4 iphase: 0) round: 0.5)) * 99.
i := (2 ** (LFSaw freq: 1 / #(8 9) iphase: 0)) * 8.
g := GrainFM numChan: 1 trigger: t dur: 16 carfreq: f modfreq: m index: i pan: 0 envbufnum: -1 maxGrains: 512.
(g / 2) tanh * 0.25
