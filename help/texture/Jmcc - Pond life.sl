;; pond life ; jmcc ; #1
{
	var f = SinOsc(20 + 30.Rand, 0) * (100 + 300.Rand) + 500 + LinRand(0, 2000, 0);
	var o = SinOsc(f, 0) * LfPulse(3 / (1 + 8.Rand), 0, 0.2 + 0.3.Rand);
	Pan2(o, 1.Rand2, 0.04)
}.overlap(8, 8, 4)
