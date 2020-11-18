"envPerc"
|a p e|
a := 0.1 .
p := EnvPerc attackTime: 0.01 releaseTime: 1 level: 1 curves: #(-4 -4) .
e := EnvGen gate: 1 levelScale: a levelBias: 0 timeScale: 1 doneAction: 2 envelope: p envelope coord .
SinOsc freq: 440 phase: 0 mul: e kr

"---- coord"
e := EnvPerc attackTime: 0.01 releaseTime: 1 level: 1 curves #(-4 -4) .
e envelope coord = #(0 2 -99 -99 1 0.01 5 -4 0 1 5 -4)
