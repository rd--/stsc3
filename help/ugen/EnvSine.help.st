"EnvSine"
| e |
e := EnvSine dur: 9 level: 0.1.
SinOsc freq: 440 phase: 0 mul: (e envGen: 1 doneAction: 2) kr

"EnvSine ; trigger"
| tr e |
tr := MouseButton minval: 0 maxval: 1 lag: 0.2.
e := EnvSine dur: 1 level: 0.1.
SinOsc freq: (TRand lo: 220 hi: 440 trig: tr) phase: 0 mul: (e envGen: tr)

"---- ; coordinates"
| e |
e := EnvSine dur: 3 level: 1
e env coord = #(0 2 -99 -99 1 1.5 5 3 0 1.5 5 3)
