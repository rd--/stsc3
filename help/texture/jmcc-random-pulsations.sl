;; random pulsations (jmcc) #1
{
	var o1 = SinOsc(Rand(0, 2000), 0) * 0.02;
	var o2 = SinOsc(LinRand(8, 88, 0), 0);
	var o3 = SinOsc(0.3 + Rand(0, 0.5), Rand(0, 2 * pi)) * 0.7;
	Pan2(Release(o1, 2, 5, 2).AmClip(o2), o3, 1)
}.playEvery(9 / 8)
