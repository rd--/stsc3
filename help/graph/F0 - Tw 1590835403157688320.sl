(* https://twitter.com/redFrik/status/1590835403157688320 *)
var b = [1 .. 9];
var g = 1 / b / 99;
var z = Latch(
	WhiteNoise().RoundTo(0.5) + 2,
	BrownNoise() + 1.4 > SinOsc(b / 999, 0)
);
var r = Rlpf(
	z,
	9 ^ SinOsc(g * 9, 0) * 999,
	SinOsc(b / 77, 0) / 2 + 0.6
);
var x = SinOsc(999 * b.scramble / r, 0);
var w = Latch(
	WhiteNoise(),
	BrownNoise() + 1.3 > SinOsc(b / 99, 0)
);
var y = Rlpf(
	w,
	9 ^ SinOsc(g, 0) * 999,
	SinOsc(b / 88, 0) / 2 + 0.6
);
Splay(7 ^ SinOsc(g, 0) / 9 * x * y)
