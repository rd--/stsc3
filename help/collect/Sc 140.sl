(* sc-140 ; 01 ; Nathaniel Virgo *)
var a =
	CombN(
		Bpf(
			LocalIn(2, 0) * 7.5 + (Saw([32, 33]) * 0.2),
			2 ^ (LfNoise0(4 / 3) * 4) * 300,
			0.1
		).Distort,
		2,
		2,
		40
	);
a <! LocalOut(a)

(* sc-140 ; 03 ; Tim Walters *)
(0 .. 15).collect { :k |
	(0 .. 7).collect { :i |
		var e = Decay(Dust(1 / 4 ^ i), SinOsc(0.1, 0) + 1 * k + i) * k * 999;
		var ph = SinOsc(i * k ^ i / [4, 5], 0) * e;
		SinOsc(i * k * k, ph)
	}.product
}.Mix

(* sc-140 ; 05 ; Batuhan Bozkurt *)
var f = LocalIn(2, 0).Tanh;
var k = Latch(f[1].Abs, Impulse(1 / 4, 0));
f <! LocalOut(f + CombC(Blip([4, 6], 100 * k + 50) * 0.9, 1, k * 0.3, 50 * f))

(* sc-140 ; 07 ; Thor Magnusson *)
var a = LfNoise0(8);
EqPan2([
	SinOsc(Pulse(1, 0.5) * 24, 0),
	SinOsc(90 + (a * 90), 0),
	MoogFf(Saw(LfNoise0(4) * 333 + 666), a * XLine(1, 39, 99) * 99, 2, 0)
].Sum, 0) / 3
