;; bowed garage door springs (jmcc) #9
{
	var exc = PinkNoise() * (LfNoise1(3.0.rand) * 0.0008 + 0.0022);
	var sig = RingzBank(exc, { 50.0.rrand(2000) } ! 4, nil, { 0.2.rrand(12) } ! 4).abs * [-1, 1].atRandom;
	var z = Pan2(sig, LfNoise1(1.0.rand), 1);
	6.timesRepeat { z := AllpassN(z, 0.040, { 0.040.rand } ! 2, 16) };
	z
}.overlap(8, 3, 4)
