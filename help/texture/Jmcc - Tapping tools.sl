(* tapping tools (jmcc) #7 *)
{
	var rate = XLine(64, 0.125, 60);
	var exc = Decay(Impulse(LinRand(1, 20, 0) * rate, 0) * 0.03, 0.001);
	var flt = RingzBank(
		exc,
		{ 400 + 8000.Rand } ! 4,
		nil,
		{ 0.01 + 0.1.Rand } ! 4
	);
	var z = Release(EqPan(flt, 1.Rand2) * 0.25, 1, 4, 1);
	3.timesRepeat {
		z := AllpassN(z, 0.05, { 0.05.Rand } ! 2, 2)
	};
	z
}.playEvery(2)
