;; birdies (jmcc) #6
{
	var p1 = LFPulse(0.4 + Rand(0, 1), 0, Rand(0, 0.8) + 0.1) * Rand(4, 7) + 2;
	var p2 = LFPulse(0.4 + Rand(0, 1), 0, Rand(0, 0.8) + 0.1) * Rand(4, 7);
	var p3 = LFPulse(0.2 + Rand(0, 0.5), 0, 0.4);
	var sw = LFSaw(p1 + p2, 0) * (1000 + Rand(0, 800)).negated + 4000 + Rand(-1200, 1200);
	var freq = Lag(sw, 0.05);
	var amp = Lag(p3, 0.3);
	Pan2(SinOsc(freq, 0) * amp, Rand(-1, 1), 0.02)
}.overlap(7, 4, 4)

;; birdies (jmcc) #6 ; muladd
{
	var p1 = MulAdd(LFPulse(0.4 + Rand(0, 1), 0, Rand(0, 0.8) + 0.1), Rand(0, 3) + 4, 2);
	var p2 = MulAdd(LFPulse(0.4 + Rand(0, 1), 0, Rand(0, 0.8) + 0.1), Rand(0, 3) + 4, 0);
	var p3 = MulAdd(LFPulse(0.2 + Rand(0, 0.5), 0, 0.4), 0.02, 0);
	var sw = MulAdd(LFSaw(p1 + p2, 0), (1000 + Rand(0, 800)).negated, 4000 + Rand(-1200, 1200));
	var freq = Lag(sw, 0.05);
	var amp = Lag(p3, 0.3);
	Pan2(SinOsc(freq, 0) * amp, Rand(-1, 1), 1)
}.overlap(7, 4, 4)
