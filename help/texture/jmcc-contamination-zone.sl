;; contamination zone (jmcc) #9
{
	var f = ExpRand(800, 8000);
	var pan = LFNoise1(Rand(0, 1));
	var amp = LFPulse(LinRand(0, 15, 0), 0, 0.2 + Rand(0, 0.2));
	var exc = PinkNoise() * (LFNoise1(Rand(0, 3)) * 0.0008 + 0.0022);
	var sig = RingzBank(exc, { Rand(50, 2000) } ! 4, nil, { Rand(0.2, 4) } ! 4).abs * [-1, 1].atRandom;
	var flt = RLPF(sig, SinOsc(LinRand(0, 1, 0), 0) * 0.7 * f + f, 0.1);
	var z = Pan2(flt * amp, pan, 1);
	6.timesRepeat { z := AllpassN(z, 0.1, { Rand(0, 0.05) } ! 2, 4) };
	z
}.overlap(8, 3, 4)
