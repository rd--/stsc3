"envASR ; random release time"
|g e|
g := SetResetFF trig: 1 reset: (Dust density: 1) .
e := EnvASR attackTime: 0.01 sustainLevel: 0.1 releaseTime: 1 curves: #(-4 -4) .
SinOsc freq: 440 phase: 0 mul: (e envGen: g doneAction: 2) kr

"---- ; coordinates"
|e|
e := EnvASR attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curves: #(-4 -4) .
e env coord = #(0 2 1 -99 1 0.01 5 -4 0 1 5 -4)
