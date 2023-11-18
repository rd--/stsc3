(* sample and hold liquidities (jmcc) #4 *)
var r = MouseX(1, 200, 1, 0.1);
var t = r.Recip;
var c = Impulse(r, 0) * 0.4;
var cf = MouseY(100, 8000, 1, 0.1);
var i = EqPan(
	SinOsc(Latch(WhiteNoise() * cf * 0.5 + cf, c), 0),
	Latch(WhiteNoise(), c)
) * Decay2(c, 0.1 * t, 0.9 * t);
CombN(i, 0.3, 0.3, 2)
