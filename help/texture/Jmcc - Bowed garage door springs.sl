(* bowed garage door springs (jmcc) #9 *)
{
	var exc = PinkNoise() * (LfNoise1(3.Rand) * 0.0008 + 0.0022);
	var sig = RingzBank(
		exc,
		{ Rand(50, 2000) } ! 4,
		nil,
		{ Rand(0.2, 12) } ! 4
	).Abs * [-1, 1].atRandom;
	var z = EqPan(sig, LfNoise1(1.Rand));
	6.timesRepeat {
		z := AllpassN(z, 0.04, { 0.04.Rand } ! 2, 16)
	};
	z
}.overlap(8, 3, 4)
