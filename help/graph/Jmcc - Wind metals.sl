(* wind metals (jmcc) ; graph rewrite *)
{ :tr |
	var n = 6;
	var exc = [
		{ BrownNoise() } ! 2,
		0.007,
		(LfNoise1(ExpRand(tr, 0.125, 0.5)) * 0.75 + 0.25).Max(0)
	].product;
	var f = {
		[
			Rand(tr, 0, Rand(tr, 500, 8000)),
			ExpRand(tr, 60, 4000)
		].sum
	} ! n;
	var dt = { Rand(tr, 0.1, 2) } ! n;
	var s = RingzBank(exc, f, [1], dt) * 0.1;
	s.SoftClip
}.OverlapTexture(5, 2, 12).Mix * 0.1
