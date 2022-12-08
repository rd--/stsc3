;; birdies (jmcc) #6
{
	var p1 = LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1) * Rand(4, 7) + 2;
	var p2 = LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1) * Rand(4, 7);
	var p3 = LfPulse(0.2 + 0.5.Rand, 0, 0.4);
	var sw = LfSaw(p1 + p2, 0) * (1000 + 800.Rand).negated + 4000 + 1200.Rand2;
	var freq = Lag(sw, 0.05);
	var amp = Lag(p3, 0.3);
	Pan2(SinOsc(freq, 0) * amp, 1.Rand2, 0.02)
}.overlap(7, 4, 4)

;; birdies (jmcc) #6 ; muladd
{
	var p1 = MulAdd(LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1), 3.Rand + 4, 2);
	var p2 = MulAdd(LfPulse(0.4 + 1.Rand, 0, 0.8.Rand + 0.1), 3.Rand + 4, 0);
	var p3 = MulAdd(LfPulse(0.2 + 0.5.Rand, 0, 0.4), 0.02, 0);
	var sw = MulAdd(LfSaw(p1 + p2, 0), (1000 + 800.Rand).negated, 4000 + 1200.Rand2);
	var freq = Lag(sw, 0.05);
	var amp = Lag(p3, 0.3);
	Pan2(SinOsc(freq, 0) * amp, 1.Rand2, 1)
}.overlap(7, 4, 4)
