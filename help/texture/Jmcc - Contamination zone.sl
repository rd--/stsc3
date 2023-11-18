(* contamination zone (jmcc) #9 *)
{
	var f = ExpRand(800, 8000);
	var pan = LfNoise1(1.Rand);
	var amp = LfPulse(LinRand(0, 15, 0), 0, 0.2 + 0.2.Rand);
	var exc = PinkNoise() * (LfNoise1(3.Rand) * 0.0008 + 0.0022);
	var sig = RingzBank(
		exc,
		{ Rand(50, 2000) } ! 4,
		nil,
		{ Rand(0.2, 4) } ! 4
	).Abs * [-1, 1].atRandom;
	var flt = Rlpf(sig, SinOsc(LinRand(0, 1, 0), 0) * 0.7 * f + f, 0.1);
	var z = EqPan(flt * amp, pan);
	6.timesRepeat {
		z := AllpassN(z, 0.1, { 0.05.Rand } ! 2, 4)
	};
	z
}.overlap(8, 3, 4)
