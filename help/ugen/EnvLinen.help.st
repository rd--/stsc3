"EnvLinen"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(#lin #lin #lin).
SinOsc freq: 440 phase: 0 mul: (e envGen: 1 doneAction: 2) kr

"EnvLinen ; trigger ; mouse button control"
| tr e |
tr := MouseButton minval: 0 maxval: 1 lag: 0.
e := EnvLinen attackTime: 0.1 sustainTime: 0.1 releaseTime: 4 level: 1 curves: #(#lin #lin #lin).
SinOsc freq: (TRand lo: 110 hi: 440 trig: tr) phase: 0 mul: (e envGen: tr) * 0.1

"---- ; coordinates"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(#lin #lin #lin).
e env coord = #(0 3 -99 -99 0.1 0.4 1 0 0.1 2 1 0 0 0.4 1 0)
