"envPerc"
| e |
e := EnvPerc attackTime: 0.01 releaseTime: 1 level: 0.1 curves: #(-4 -4).
SinOsc freq: 440 phase: 0 mul: (e envGen: 1 doneAction: 2) kr

"EnvPerc ; gate ; mouse button control"
| e g |
e := EnvPerc attackTime: 0.01 releaseTime: 1 level: 0.1 curves: #(-4).
g := MouseButton minval: 0 maxval: 1 lag: 0.
SinOsc freq: (TRand lo: 220 hi: #(330 440) trig: g) phase: 0 mul: (e envGen: g)

"---- ; coordinates"
| e |
e := EnvPerc attackTime: 0.01 releaseTime: 1 level: 1 curves: #(-4 -4).
e env coord = #(0 2 -99 -99 1 0.01 5 -4 0 1 5 -4)
