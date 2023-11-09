(* sawed cymbals (jmcc) ; #10 ; graph rewrite *)
var p = 15;
{ :tr |
	var f1 = TrRand(tr, 500, 2500);
	var f2 = TrRand(tr, 0, 8000);
	var f3 = TrXLine(tr, TrRand(tr, 0, 600), TrRand(tr, 0, 600), 12);
	var s = LfSaw(f3, 0) * 0.0005;
	{
		RingzBank(
			s,
			{ f1 + TrRand(tr, 0, f2) } ! p,
			nil,
			{ TrRand(tr, 2, 6) } ! p
		)
	} ! 2 * 0.1
}.OverlapTexture(4, 4, 6)
