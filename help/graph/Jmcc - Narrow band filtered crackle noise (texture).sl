(* narrow band filtered crackle noise (jmcc) #2 ; graph rewrite *)
{ :tr |
	var rf1 = TrRand(tr, 0, 2000) + 80;
	var rf2 = rf1 + (TrRand(tr, -0.2, 0.2) * rf1);
	var rf = TrXLine(tr, rf1, rf2, 9);
	var c = Crackle(1.97 + TrRand(tr, 0, 0.03));
	EqPan2(Resonz(c, rf, 0.2), TrRand(tr, -1, 1))
}.OverlapTexture(2, 5, 4) * 0.15 (* c.f. 5, 2, 5 *)
