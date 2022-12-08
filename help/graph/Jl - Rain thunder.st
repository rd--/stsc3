"http://sccode.org/1-e (jl)"
| d0 n0 e0 p0 n1 f1 e1 p1 |
d0 := Lpf in: (Dust2 density: (LfNoise1 freq: 0.2 mul: 5 add: 45) kr) freq: 7000.
n0 := PinkNoise mul: (LfNoise1 freq: 0.3 mul: 0.02 add: 0.08) add: d0.
e0 := Line start: 0 end: 1 dur: 10 doneAction: 0.
p0 := (GVerb in: (Hpf in: (n0 + d0) freq: 400) roomsize: 250 revtime: 100 damping: 0.25 inputbw: 0.5 spread: 15 drylevel: 0.3 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300 mul: e0 kr) tanh.
n1 := PinkNoise mul: ((Clip in: (LfNoise1 freq: 3) lo: 0 hi: 1 mul: (Clip in: (LfNoise1 freq: 2) lo: 0 hi: 1)) kr ** 1.8).
f1 := LinExp in: (LfNoise1 freq: 1) srclo: -1 srchi: 1 dstlo: 100 dsthi: 2500.
e1 := Line start: 0 end: 0.7 dur: 30 doneAction: 0.
p1 := GVerb in: (Lpf in: (Hpf in: n1 freq: 20 mul: 10) freq: f1 kr) tanh roomsize: 270 revtime: 30 damping: 0.7 inputbw: 0.5 spread: 15 drylevel: 0.5 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300 mul: e1 kr.
Limiter in: (p0 + p1) level: 1 dur: 0.01 mul: 0.2
