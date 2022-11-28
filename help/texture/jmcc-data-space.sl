;; data space ; jmcc ; #2
{
	var dt = 0.25.rand + 0.1;
	var osc = { :n :m |
		var e = LfPulse(m.rand, 0, 1.0.rand) * 8000.rand + 2000.rand;
		LfPulse(n.rand, 0, 1.0.rand) * e
	};
	var freq = osc(200.0, 40.0) + osc(20.0, 4.0) + osc(20.0, 4.0);
	CombL(Pan2(LfPulse(freq, 0, 0.5), LfNoise0(3.0.rand) * 0.8, 0.04), dt, dt, 3)
}.overlap(6, 1, 4)
