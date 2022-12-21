;; Ln
SinOsc([110, Ln(110, 440, 100)], 0) * 0.1

;; Ln
SinOsc(Ln(200, 17000, 5), 0) * 0.1

;; Ln
SinOsc(Ln(200, [209, 211], 5), 0) * 0.1

;; Ln
var chord = { :m0 |
	var m1 = m0 + [0, 4.078, 7.019, 11.097];
	var m2 = m0 + [0, 4.980, 7.921, 10.863];
	var du = 90;
	var o = (1 .. 4).collect { :index |
		EqPan2(
			SinOsc(Ln(m1[index], m2[index], du).MidiCps, 0),
			Ln(1.Rand2, 1.Rand2, du)
		) * Ln(0.1.Rand, 0.1.Rand, du)
	};
	o.sum
};
var octaves = 12 * (5 .. 7);
octaves.collect(chord).sum * 0.2
