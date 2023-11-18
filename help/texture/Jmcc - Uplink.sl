(* uplink (jmcc) #2 *)
{
	var osc = {
		var e = LfPulse(4.Rand, 0, 1.Rand) * 8000.Rand + 2000.Rand;
		LfPulse(20.Rand, 0, 1.Rand) * e
	};
	var freq = osc() + osc();
	EqPan(LfPulse(freq, 0, 0.5), 0.8.Rand2) * 0.04
}.overlap(4, 1, 5)
