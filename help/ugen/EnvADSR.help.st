"envADSR ; random release time"
|g p e|
g := SetResetFF trig: 1 reset: (Dust density: 1) .
p := EnvADSR attackTime: 0.01 decayTime: 0.25 sustainLevel: 0.15 releaseTime: 0.75 peakLevel: 1 curves: #(-4 -4 -4) .
e := EnvGen gate: g levelScale: 0.1 levelBias: 0 timeScale: 1 doneAction: 2 envelope: p envelope coord .
SinOsc freq: 440 phase: 0 mul: e kr

"---- ; coordinates"
|e|
e := EnvADSR attackTime: 0.01 decayTime: 0.25 sustainLevel: 0.15 releaseTime: 0.75 peakLevel: 1 curves: #(-4 -4 -4) .
e envelope coord = #(0 3 2 -99 1 0.01 5 -4 0.15 0.25 5 -4 0 0.75 5 -4)
