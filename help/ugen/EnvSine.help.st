"EnvSine"
| e |
e := EnvSine dur: 9 level: 0.1.
SinOsc freq: 440 phase: 0 mul: (e envGen: 1 doneAction: 2) kr

"---- ; coordinates"
| e |
e := EnvSine dur: 3 level: 1
e env coord = #(0 2 -99 -99 1 1.5 5 3 0 1.5 5 3)
