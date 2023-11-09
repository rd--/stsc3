(* repeating inharmonic klank ; jmcc #6 ; graph rewrite *)
var n = 4;
var p = 8;
{ :tr |
	var s = Decay(Dust(0.8) * 0.004, 3.4) * LfSaw(TrRand(tr, 0, 40), 0);
	{
		RingzBank(
			s,
			{ TrRand(tr, 80, 10000) } ! p,
			1,
			{ TrRand(tr, 0.4, 4.4) } ! p
		)
	} ! 2
}.OverlapTexture(8, 8, n)
