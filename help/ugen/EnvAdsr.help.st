"EnvAdsr ; random release time"
| g e |
g := SetResetFF trig: 1 reset: (Dust density: 1).
e := EnvAdsr attackTime: 0.01 decayTime: 0.25 sustainLevel: 0.15 releaseTime: 0.75 peakLevel: 0.1 curves: -4.
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: g withDoneAction: 2) kr

"EnvAdsr ; mouse button control"
| g e |
g := MouseButton minval: 0 maxval: 1 lag: 0.2.
e := EnvAdsr attackTime: 0.75 decayTime: 1.75 sustainLevel: 0.35 releaseTime: 3.25 peakLevel: 1 curves: -4.
SinOsc freq: 440 phase: 0 mul: (EnvGen gate: g levelScale: 0.1 levelBias: 0 timeScale: 1 doneAction: 0 envelope: e asArray)

"---- ; coordinates"
| env |
env := EnvAdsr attackTime: 0.01 decayTime: 0.25 sustainLevel: 0.15 releaseTime: 0.75 peakLevel: 1 curves: -4.
env asArray = #(0 3 2 -99 1 0.01 5 -4 0.15 0.25 5 -4 0 0.75 5 -4)
