"Env"
| e |
e := Env levels: #(0.1 0) times: #(1) curves: #(-4).
SinOsc freq: 440 phase: 0 mul: (e gen: 1 doneAction: 2) kr

"Env"
| e |
e := Env levels: #(1000 20) times: #(1).
Saw freq: e gen mul: 0.1

"Env"
| freqEnv ampEnv |
freqEnv := Env levels: #(1000 20) times: #(0.5).
ampEnv := Env levels: #(0.1 0) times: #(2).
Saw freq: freqEnv gen mul: ampEnv gen

"Env"
Saw
  freq: (Env levels: #(1000 20) times: #(0.5)) gen kr
  mul: (Env levels: #(0.1 0) times: #(2)) gen kr

"Env ; FM"
SinOsc
  freq: (SinOsc freq: 10 phase: 0) * 10 + 440
  phase: 0
  mul: ((Env levels: #(0.5 0) times: #(1)) gen: 1 doneAction: 2) kr
