"EnvSpec"
| e |
e := EnvSpec levels: #(0.1 0) times: #(1) curves: #(-4).
SinOsc freq: 440 phase: 0 mul: (e gen: 1 doneAction: 2) kr

"EnvSpec"
| e |
e := EnvSpec levels: #(1000 20) times: #(1).
Saw freq: e gen mul: 0.1

"EnvSpec"
| freqEnv ampEnv |
freqEnv := EnvSpec levels: #(1000 20) times: #(0.5).
ampEnv := EnvSpec levels: #(0.1 0) times: #(2).
Saw freq: freqEnv gen mul: ampEnv gen

"EnvSpec"
Saw
  freq: (EnvSpec levels: #(1000 20) times: #(0.5)) gen kr
  mul: (EnvSpec levels: #(0.1 0) times: #(2)) gen kr

"EnvSpec ; FM"
SinOsc
  freq: (SinOsc freq: 10 phase: 0) * 10 + 440
  phase: 0
  mul: ((EnvSpec levels: #(0.5 0) times: #(1)) gen: 1 doneAction: 2) kr
