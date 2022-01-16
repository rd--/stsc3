"envASR ; random release time"
| g e |
g := SetResetFF trig: 1 reset: (Dust density: 1).
e := EnvASR attackTime: 0.01 sustainLevel: 0.1 releaseTime: 1 curve: -4.
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: g withDoneAction: 2) kr

"AR ; event control"
| f |
f := [ :e |
  env := AR gate: e w attackTime: 0.5 * e y releaseTime: e y * 4 curve: -4.
  SinOsc freq: e p unitCps phase: 0 mul: env * 0.1
].
(Voicer numVoices: 16 voiceFunc: f) splay2

"---- ; coordinates"
| e |
e := EnvASR attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curve: -4.
e asEnvSpec coord = #(0 2 1 -99 1 0.01 5 -4 0 1 5 -4)
