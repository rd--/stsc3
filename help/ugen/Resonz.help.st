"resonator"
Resonz in: (LFNoise0 freq: 400) freq: (Line start: 10000 end: 1000 dur: 10 doneAction: 0) bwr: 0.1

"resonz ; pinkNoise ; event control"
| f |
f := [ :e |
  | snd |
  snd := Resonz in: PinkNoise new freq: (e p * 127 + e px) midiCps bwr: e y * 0.25 mul: 24.
  Pan2 in: snd pos: e o * 2 - 1 level: e z squared * e w ].
(Voicer numVoices: 16 voiceFunc: f) mix
