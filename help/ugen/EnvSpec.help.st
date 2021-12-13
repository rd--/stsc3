"EnvSpec"
| e |
e := EnvSpec levels: #(0.1 0) times: #(1) curves: #(-4).
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: 1 withDoneAction: 2) kr

"EnvSpec"
| e |
e := EnvSpec levels: #(1000 20) times: #(1).
Saw freq: e asEnvGen mul: 0.1

"EnvSpec"
| freqEnv ampEnv |
freqEnv := EnvSpec levels: #(1000 20) times: #(0.5).
ampEnv := EnvSpec levels: #(0.1 0) times: #(2).
Saw freq: freqEnv asEnvGen mul: ampEnv asEnvGen

"EnvSpec"
Saw
  freq: (EnvSpec levels: #(1000 20) times: #(0.5)) asEnvGen kr
  mul: (EnvSpec levels: #(0.1 0) times: #(2)) asEnvGen kr

"EnvSpec ; FM"
SinOsc
  freq: (SinOsc freq: 10 phase: 0) * 10 + 440
  phase: 0
  mul: ((EnvSpec levels: #(0.5 0) times: #(1)) asEnvGen: 1 withDoneAction: 2) kr

"EnvSpec ; as triangle wave oscillator"
| f e |
f := MouseX minval: 110 maxval: #(220 330) warp: 0 lag: 0.2.
e := EnvSpec levels: #(0 0 1 0 -1) times: #(0.25 0.25 0.25 0.25) curves: #(#lin) releaseNode: nil loopNode: 0 offset: 0.
EnvGen gate: (Impulse freq: f phase: 0) levelScale: 0.1 levelBias: 0 timeScale: 1 / f doneAction: 0 envelope: e coord
