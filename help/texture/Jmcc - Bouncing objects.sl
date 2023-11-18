(* bouncing objects (jmcc) #2 *)
{
	var impFreq = XLine(3 + 2.Rand, 600, 4);
	var impAmp = XLine(0.01 + 0.09.Rand, 0.000009, 4);
	var imp = Impulse(impFreq, 0) * impAmp;
	var exc = Decay(imp, 0.001);
	var fltFreq = { 400 + 8000.Rand } ! 4;
	var fltAmp = { 1.Rand } ! 4;
	var fltRtm = { 0.01 + 0.1.Rand } ! 4;
	var flt = RingzBank(exc, fltFreq, fltAmp, fltRtm);
	EqPan(Release(flt, 0, 3, 0.001), 1.Rand2)
}.playEvery { 0.6 + 0.6.randomFloat }
