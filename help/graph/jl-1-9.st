"http://sccode.org/1-9 (jl)"
| n k a f |
n := 956. "0"
k := 98. "1"
a := HPF in: (PinkNoise mul: 0.005) freq: 10 mul: (Ln start: 0 end: 1 dur: 9) kr.
f := [ :i | Ringz in: (LFNoise1 freq: (0.05 + 0.1 rand) mul: a) freq: 55 * (i + n) + 60 decaytime: 0.2 ].
(GVerb in: (1 to: k collect: f) sum roomsize: 70 revtime: 99 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) tanh
