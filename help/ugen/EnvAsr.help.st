"envAsr ; random release time"
| g e |
g := SetResetFF trig: 1 reset: (Dust density: 1).
e := EnvAsr attackTime: 0.01 sustainLevel: 0.1 releaseTime: 1 curves: -4.
SinOsc freq: 440 phase: 0 mul: (e asEnvGen: g withDoneAction: 2) kr

"Asr ; event control"
| f |
f := [ :e |
	env := Asr gate: e w attackTime: 0.5 * e y releaseTime: e y * 4 curves: -4.
	SinOsc freq: e p unitCps phase: 0 mul: env * 0.1
].
(Voicer numVoices: 16 voiceFunc: f) splay2

"---- ; coordinates"
| env |
env := EnvAsr attackTime: 0.01 sustainLevel: 1 releaseTime: 1 curves: -4.
env asArray = #(0 2 1 -99 1 0.01 5 -4 0 1 5 -4)
