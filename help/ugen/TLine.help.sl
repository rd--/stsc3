(* TLine *)
var tr = Impulse(1, 0);
var f0 = Rand(tr, 220, 330);
var f1 = Rand(tr, 110, 440);
var dur = Rand(tr, 0.1, 1);
var f = Line(tr, f0, f1, dur);
var e = Sine(tr, dur) * 0.1;
SinOsc(f, 0) * e

(* TLine ; https://scsynth.org/t/6371/12 *)
var x = MouseX(10, 100, 1, 0.2);
var y = MouseY(0.01, 0.05, 1, 0.2);
{
	var i = Impulse(x + (x * LfNoise2(0.1) * 0.1), 0);
	var m = Rand(i, 0.95, 1.05);
	var t = Line(i, -1.5, 1.5, 0.01 * m);
	var c = ((10 * (2 * pi * 5 * t).Cos) + (5 * (2 * pi * 40 * t).Cos)) * (pi.negated * t.Squared).Exp;
	c * y
} ! 2
