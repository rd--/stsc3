(* bowed string (jmcc) ; texture ; graph rewrite *)
{ :tr |
	var root = 5;
	var scale = [0 2 4 5 7 9 11] + root;
	var oct = [24 36 48 60 72 84];
	var f = (Choose(tr, scale) + Choose(tr, oct)).MidiCps;
	var e = (LfNoise1(TExpRand(0.125, 0.5, tr)) * 0.6 + 0.4).Max(0);
	var x = { BrownNoise() } ! 2 * 0.007 * e;
	var k = DynRingzBank(
		x,
		12.arithmeticSeries(f, f),
		12.geometricSeries(1, TRand(0.7, 0.9, tr)),
		{ TRand(1, 3, tr) } ! 12
	);
	EqPan2(k.SoftClip, TRand(-1, 1, tr)) / 10
}.OverlapTexture(5, 2, 9).Mix
