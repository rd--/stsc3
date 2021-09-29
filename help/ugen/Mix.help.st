"Mix"
{PinkNoise mul: 0.1. SinOsc freq: 801 phase: 0 mul: 0.1. LFSaw freq: 40 iphase: 0 mul: 0.1} mce mix

"Mix"
(8 mceFill: [ :ix | SinOsc freq: 500 + 500.0 rand phase: 0 mul: 0.05 ]) mix

"Mix"
8 mixFill: [ :ix | SinOsc freq: 500 + 500.0 rand phase: 0 mul: 0.05 ]
