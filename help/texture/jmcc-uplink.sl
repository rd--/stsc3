;; uplink (jmcc) #2
{
	var osc = {
		var e = LfPulse(4.0.rand, 0, 1.0.rand) * 8000.rand + 2000.rand;
		LfPulse(20.0.rand, 0, 1.0.rand) * e
	};
	var freq = osc() + osc();
	Pan2(LfPulse(freq, 0, 0.5), 0.8.rand2, 0.04)
}.overlap(4, 1, 5)
