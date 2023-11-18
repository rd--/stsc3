(* tremulate (jmcc) ; graph rewrite *)
{ :tr |
	var f = Rand(tr, 400, 900);
	var r = Rand(tr, 60, 90) ! 4;
	var o = SinOsc(f * [1 1.2 1.5 1.8], 0); (* just minor seventh chord *)
	var e = (LfNoise2(r) * 0.1).Max(0);
	EqPan2(o * e, { Rand(-1, 1) } ! 4).sum
}.OverlapTexture(2, 0.5, 2).Mix.CombN(0.1, 0.1, 1)
