(* Sos *)
Voicer(16) { :e |
	var b1 = LinLin(e.y, 0, 1, 1.45, 1.998);
	var b2 = LinLin(e.x, 0, 1, -0.999, -0.9998);
	EqPan2(Sos(Trig(K2A(e.w), 0), 0.0, 0.05, 0.0, b1, b2), e.i * 2 - 1) * 8
}.Mix

(* Sos *)
Voicer(16) { :e |
	var freq = (e.x * 25 + 48).MidiCps;
	var rq = 0.002 + (e.y * 0.004);
	var b1 = 1.987 * 0.9889999999 * 0.09.cos;
	var b2 = -0.998057;
	var im = Decay(Trig1(K2A(e.w), SampleDur()), 0.1);
	var s1 = Sos(im, 0.3, 0.0, 0.0, b1, b2);
	var s2 = Rhpf(s1 * 0.8, freq, rq);
	var s3 = s2 + DelayC(Rhpf (s1 * 0.9, freq * 0.99999, rq * 0.999), 0.02, 0.01223);
	var s4 = Decay2(s3, 0.4, 0.3) * s2;
	EqPan2(s4, e.i * 2 - 1) * e.z * 0.25
}.Mix
