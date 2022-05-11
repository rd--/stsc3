"sum"
{PinkNoise mul: 0.1. SinOsc freq: 801 phase: 0 mul: 0.1. LFSaw freq: 40 iphase: 0 mul: 0.1} sum

"sum"
(SinOsc freq: #(440 441) phase: 0 mul: 0.1) sum

"mceFill ; sum"
([ SinOsc freq: 500 + 500.0 rand phase: 0 mul: 0.05 ] dup: 8) sum

"mixFill"
([ SinOsc freq: 500 + 500.0 rand phase: 0 mul: 0.05 ] dup: 8) sum
