(* resonant dust (jmcc) #2 ; graph rewrite *)
{ :tr |
	var rf1 = TRand(0, 2000, tr) + 80;
	var rf2 = rf1 + (TRand(-0.5, 0.5, tr) * rf1);
	var d = Dust(50 + TRand(0, 800, tr));
	var s = Resonz(d, TxLine(rf1, rf2, 9, tr), 0.1);
	EqPan2(s, TRand(-1, 1, tr)) * 0.3
}.OverlapTexture(5, 2, 4).Mix
