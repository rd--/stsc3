(* bowed string (jmcc) ; texture ; graph rewrite *)
{ :tr |
	var root = 5;
	var scale = [0 2 4 5 7 9 11] + root;
	var oct = [24 36 48 60 72 84];
	var f = (Choose(tr, scale) + Choose(tr, oct)).MidiCps;
	var e = (LfNoise1(ExpRand(tr, 0.125, 0.5)) * 0.6 + 0.4).Max(0);
	var x = { BrownNoise() } ! 2 * 0.007 * e;
	var k = DynRingzBank(
		x,
		12.arithmeticSeries(f, f),
		12.geometricSeries(1, Rand(tr, 0.7, 0.9)),
		{ Rand(tr, 1, 3) } ! 12
	);
	Pan2(k.SoftClip, Rand(tr, -1, 1), 0.1)
}.OverlapTexture(5, 2, 9).Mix
