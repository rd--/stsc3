(* hell is busy (jmcc) #1 *)
{
	var e = LfPulse(1 + 10.Rand, 0, 0.7.Rand) * 0.04;
	EqPan(SinOsc(400 + 2000.Rand, 0) * e, 1.Rand2)
}.overlap(4, 4, 8)
