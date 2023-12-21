"pluck ; event control"
| f |
f := [ :e |
	| n dm dl sig |
	n := WhiteNoise mul: e z.
	dm := 1 / 220.
	dl := (e x negated * 0.9 + 1) * dm.
	sig := Pluck in: n trig: e w maxdelaytime: dm delaytime: dl decaytime: 10 coef: (e y / 3).
	Pan2 in: sig pos: e i * 2 - 1 level: 2
].
(Voicer numVoices: 16 voiceFunc: f) sum
