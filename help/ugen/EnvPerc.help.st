"envPerc"
|a p e|
a := 0.1 .
p := EnvPerc attackTime: 0.01 releaseTime: 1 level: 1 curve: -4 .
e := EnvGen gate: 1 levelScale: a levelBias: 0 timeScale: 1 doneAction: 1 envelope: p coord .
SinOsc freq: 440 phase: 0 mul: e kr

