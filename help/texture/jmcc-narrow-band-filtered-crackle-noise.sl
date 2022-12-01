;; narrow band filtered crackle noise (jmcc) #2
{
	var rf1 = Rand(80, 2080);
	var rf = XLn(rf1, rf1 * Rand(-0.2, 0.2) + rf1, 9);
	var c = Crackle(1.97 + Rand(0, 0.03)) * 0.15;
	Pan2(Release(Resonz(c, rf, 0.2), 2, 5, 2), Rand(-1, 1), 1)
}.playEvery(2)
