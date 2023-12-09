(* wind metals (jmcc) ; graph rewrite *)
{ :tr |
	var n = 6;
	var exc = [
		{ BrownNoise() } ! 2,
		0.007,
		(LfNoise1(TExpRand(0.125, 0.5, tr)) * 0.75 + 0.25).Max(0)
	].product;
	var f = {
		[
			TRand(0, TRand(500, 8000, tr), tr),
			TExpRand(60, 4000, tr)
		].Sum
	} ! n;
	var dt = { TRand(0.1, 2, tr) } ! n;
	var s = RingzBank(exc, f, [1], dt) * 0.1;
	s.SoftClip
}.OverlapTexture(5, 2, 12).Mix * 0.1
