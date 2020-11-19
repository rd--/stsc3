"envASR ; random release time"
|g p e|
g := SetResetFF trig: 1 reset: (Dust density: 1) .
p := EnvASR attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curves: #(-4 -4) .
e := EnvGen gate: g levelScale: 0.1 levelBias: 0 timeScale: 1 doneAction: 2 envelope: p envelope coord .
SinOsc freq: 440 phase: 0 mul: e kr

"---- ; coordinates"
|e|
e := EnvASR attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curves: #(-4 -4) .
e envelope coord = #(0 2 1 -99 1 0.01 5 -4 0 1 5 -4)
