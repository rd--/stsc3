;; hell is busy (jmcc) #1
{
	var e = LFPulse(1 + 10.0.rand, 0, 0.7.rand) * 0.04;
	Pan2(SinOsc(400 + 2000.0.rand, 0) * e, 1.0.rand2, 1)
}.overlap(4, 4, 8)
