(* resonant dust (jmcc) #2 ; graph rewrite *)
{ :tr |
	var rf1 = Rand(tr, 0, 2000) + 80;
	var rf2 = rf1 + (Rand(tr, -0.5, 0.5) * rf1);
	var d = Dust(50 + Rand(tr, 0, 800));
	var s = Resonz(d, XLine(tr, rf1, rf2, 9), 0.1);
	EqPan2(s, Rand(-1, 1)) * 0.3
}.OverlapTexture(5, 2, 4)
