;; bouncing objects (jmcc) #2
{
	var impFreq = XLn(Rand(3, 7), 600, 4);
	var impAmp = XLn(0.09, 0.000009, 4);
	var imp = Impulse(impFreq, 0) * impAmp;
	var exc = Decay(imp, 0.001);
	var fltFreq = { Rand(400, 8400) } ! 4;
	var fltAmp = { Rand(0, 1) } ! 4;
	var fltRtm = { Rand(0.01, 0.11) } ! 4;
	var flt = RingzBank(exc, fltFreq, fltAmp, fltRtm);
	Pan2(Release(flt, 0, 3, 0.001), Rand(-1, 1), 1)
}.playEvery { random(0.6, 1.2) }
