;; resonant dust (jmcc) #2
{
	var rf1 = 2000.rand + 80;
	var rf2 = rf1 + (0.5.rand2 * rf1);
	var d = Dust(50 + 800.rand) * 0.3;
	var s = Resonz(d, XLn(rf1, rf2, 9), 0.1);
	Pan2(Release(s, 2, 5, 2), 1.rand2, 1)
}.playEvery(2)
