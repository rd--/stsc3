"random pulsations (jmcc) #1 ; texture:=spawn,1.125,inf"
| e o1 o2 o3 |
e := EnvLinen attackTime: 2 sustainTime: 5 releaseTime: 2 level: 0.02 curves: #(#lin #lin #lin).
o1 := FSinOsc freq: (0.0 rand: 2000.0) iphase: 0 mul: (e envGen: 1 doneAction: 2) kr.
o2 := SinOsc freq: (8 + 80.0 rand) phase: 0. "linrand"
o3 := SinOsc freq: (0.3 + (0.0 rand: 0.5)) phase: (0.0 rand: (Float pi * 2)) mul: 0.7.
Pan2 in: (o1 amclip: o2) pos: o3 kr level: 1
