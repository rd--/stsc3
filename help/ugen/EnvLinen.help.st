"EnvLinen"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curve: #lin.
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: 1 withDoneAction: 2) kr

"EnvLinen ; trigger ; mouse button control"
| tr e |
tr := MouseButton minval: 0 maxval: 1 lag: 0.
e := EnvLinen attackTime: 0.1 sustainTime: 0.1 releaseTime: 4 level: 1 curve: #lin.
SinOsc freq: (TRand lo: 110 hi: #(330 440) trig: tr) phase: 0 mul: (e asEnvGen: tr) * 0.1

"---- ; coordinates"
| e |
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curve: #lin.
e asArray = #(0 3 -99 -99 0.1 0.4 1 0 0.1 2 1 0 0 0.4 1 0)
