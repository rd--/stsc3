"sc-users, 2007-04-06 (pj) [paul jones]"
| o |
o := SinOsc freq: (LFNoise2 freq: 50 mul: 50 add: 50) phase: 0 mul: 100 add: 2000.
[ BPF in: BrownNoise new freq: o kr rq: 0.001 mul: 10 ] dup
