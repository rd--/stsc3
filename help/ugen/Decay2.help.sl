(* Decay2 ; c.f. MultiTouchPad help file *)
Voicer(16) { :e |
	var impulseFreq = Choose(e.w, [1 2 3 4 6 8 9]);
	var oscFreq = Rand(e.w, 80, 880);
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

(* Decay2 ; c.f. MultiTouchPad help file *)
var f = {
	var tr = Dust(1 / 9);
	var impulseFreq = Choose(tr, [1 2 3 4 6 8 9]);
	var oscFreq = Rand(tr, 20, 880);
	var distort = Rand(tr, 0.1, 8);
	var env = Decay2(Impulse(impulseFreq, 0), 0.01, 0.2 * Rand(tr, 0.1, 1));
	var snd = SinOsc(oscFreq, 0) * env;
	EqPan2(
		(snd * distort).Tanh / distort,
		Rand(tr, -1, 1)
	) * XLine(Impulse(0, 0) + tr, Rand(tr, 0.1, 0.4), 0.001, Rand(tr, 2, 3))
};
(f ! 16).mixByNamedRule('16×2→UoS') (* 16×2→1×2 16×2→UoS *)
