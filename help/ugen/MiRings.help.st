"MiRings ; 0=modal_resonator ; event control ; i.e. hsc3-sensel-ll -v -m 16"
| f |
f := [ :e |
	| tr |
	tr := Trig1 in: e w dur: ControlDur new.
	MiRings
		in: 0
		trig: tr
		pit: e x * 24 + 36
		struct: (Latch in: e i trig: tr) * 0.75 + 0.25
		bright: e y * 0.65 + 0.15
		damp: (Latch in: e j trig: tr) negated + 0.7
		pos: (Latch in: e k trig: tr) + 0.25
		model: 0
		poly: 1
		intern_exciter: 0
		easteregg: 0
		bypass: 0
		mul: e z
].
(Voicer numVoices: 6 voiceFunc: f) sum * 4
