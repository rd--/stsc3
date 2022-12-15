;; narrow band filtered crackle noise (jmcc) #2 ; graph rewrite
OverlapTexture({ :tr |
	var rf1 = TRand(0, 2000, tr) + 80;
	var rf2 = rf1 + (TRand(-0.2, 0.2, tr) * rf1);
	var rf = TxLine(rf1, rf2, 9, tr);
	var c = Crackle(1.97 + TRand(0, 0.03, tr));
	EqPan2(Resonz(c, rf, 0.2), TRand(-1, 1, tr))
}, 2, 5, 4) * 0.15 (* c.f. 5, 2, 5 *)
