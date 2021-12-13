"envASR ; random release time"
| g e |
g := SetResetFF trig: 1 reset: (Dust density: 1).
e := EnvASR attackTime: 0.01 sustainLevel: 0.1 releaseTime: 1 curves: #(-4 -4).
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: g withDoneAction: 2) kr

"ASR ; event control"
| f |
f := [ :e |
  env := ASR gate: e w attackTime: 0.5 * e y sustainLevel: 0.1 releaseTime: e y * 4 curves: #(-4) doneAction: 0.
  SinOsc freq: e p unitCps phase: 0 mul: env
].
(Voicer numVoices: 16 voiceFunc: f) splay

"---- ; coordinates"
| e |
e := EnvASR attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curves: #(-4 -4).
e asEnvSpec coord = #(0 2 1 -99 1 0.01 5 -4 0 1 5 -4)
