"EnvSine"
| e |
e := EnvSine dur: 9 level: 0.1.
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: 1 withDoneAction: 2) kr

"EnvSine ; trigger ; mouse button control"
| tr e |
tr := MouseButton minval: 0 maxval: 1 lag: 0.2.
e := (EnvSine dur: 1 level: 0.1) asEnvGen: tr.
SinOsc freq: (TRand lo: 220 hi: #(330 440) trig: tr) phase: 0 mul: e

"---- ; coordinates"
| e |
e := EnvSine dur: 3 level: 1
e asEnvSpec coord = #(0 2 -99 -99 1 1.5 5 3 0 1.5 5 3)
