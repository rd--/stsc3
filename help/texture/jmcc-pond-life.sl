;; pond life ; jmcc ; #1
{
	var f = SinOsc(20 + 30.0.rand, 0) * (100 + 300.0.rand) + 500 + LinRand(0.0, 2000.0, 0);
	var o = SinOsc(f, 0) * LfPulse(3 / (1 + 8.0.rand), 0, 0.2 + 0.3.rand);
	Pan2(o, 1.0.rand2, 0.04)
}.overlap(8, 8, 4)
