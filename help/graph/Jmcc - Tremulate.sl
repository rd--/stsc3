(* tremulate (jmcc) ; graph rewrite *)
{ :tr |
	var f = TRand(400, 900, tr);
	var r = TRand(60, 90, tr) ! 4;
	var o = SinOsc(f * [1 1.2 1.5 1.8], 0); (* just minor seventh chord *)
	var e = (LfNoise2(r) * 0.1).Max(0);
	EqPan2(o * e, { Rand(-1, 1) } ! 4).Sum
}.OverlapTexture(2, 0.5, 2).Mix.CombN(0.1, 0.1, 1)
