"EnvLinen"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(#lin #lin #lin).
SinOsc freq: 440 phase: 0 mul: (e envGen: 1 doneAction: 2) kr

"EnvLinen ; gate"
| e g |
e := EnvLinen attackTime: 0.1 sustainTime: 0.1 releaseTime: 4 level: 1 curves: #(#lin #lin #lin).
g := MouseButton minval: 0 maxval: 1 lag: 0.
SinOsc freq: 440 phase: 0 mul: (e envGen: g) * 0.1

"---- ; coordinates"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(#lin #lin #lin).
e env coord = #(0 3 -99 -99 0.1 0.4 1 0 0.1 2 1 0 0 0.4 1 0)
