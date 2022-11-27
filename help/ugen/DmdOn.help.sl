;; DmdOn
var t = Impulse(24, 0);
var s = Choose(inf, [
	Seq(1, [1, 2, 3, 4, 5, 4, 3, 2]),
	Choose(8, [4 .. 11])
]);
var f = DmdOn(t, 0, s * 100);
var x = MouseX(-1, 1, 0, 0.1);
var o = SinOsc([f, f + 0.7], 0);
o.cubed.cubed.ScaleNeg(x) * 0.1
