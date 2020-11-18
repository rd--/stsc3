"EnvLinen"
|t e|
t := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(lin lin lin) .
e := EnvGen gate: 1 levelScale: 1 levelBias: 0 timeScale: 1 doneAction: 1 envelope: t envelope coord .
SinOsc freq: 440 phase: 0 mul: e kr

"---- ; coordinates"
e := EnvLinen attackTime: 0.4 sustainTime: 2 releaseTime: 0.4 level: 0.1 curves: #(lin lin lin) .
e envelope coord = #(0 3 -99 -99 0.1 0.4 1 0 0.1 2 1 0 0 0.4 1 0)
