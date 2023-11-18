(* resonant dust (jmcc) #2 *)
{
	var rf1 = 2000.Rand + 80;
	var rf2 = rf1 + (0.5.Rand2 * rf1);
	var d = Dust(50 + 800.Rand) * 0.3;
	var s = Resonz(d, XLine(rf1, rf2, 9), 0.1);
	EqPan(Release(s, 2, 5, 2), 1.Rand2)
}.playEvery(2)
