(* Decay2 ; c.f. MultiTouchPad help file *)
Voicer(16) { :e |
	var impulseFreq = TChoose(e.w, [1 2 3 4 6 8 9]);
	var oscFreq = TRand(80, 880, e.w);
	var mul = (1 - e.y) * e.w;
	var pan = (e.x * 2) - 1;
	var distort = e.z * 16;
	var decay = e.z * 2;
	var trig = Impulse(impulseFreq, 0);
	var snd = SinOsc(oscFreq, 0) * Decay2(trig, 0.01, 0.2 * decay);
	EqPan2(
		(snd * distort).Tanh / distort,
		pan.Lag(0.1)
	) * mul.Lag(0.1)
}.mixByNamedRule('16×2→UoS') (* 16×2→1×2 16×2→UoS *)

