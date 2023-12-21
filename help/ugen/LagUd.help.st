"LagUd ; event control"
| f |
f := [ :e |
	| o w |
	o := SinOsc freq: (e x * 24 + 48) midiCps phase: 0.
	w := LagUd in: e w lagTimeU: 0.001 lagTimeD: 2.
	Pan2 in: o pos: e i * 2 - 1 level: e z * w * 0.5
].
(Voicer numVoices: 16 voiceFunc: f) sum
