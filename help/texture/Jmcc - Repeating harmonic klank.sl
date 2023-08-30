(* repeating harmonic klank (jmcc) *)
{
	var p = 8;
	var s = Decay(Dust(0.8) * 0.01, 3.4) * LfSaw(LinRand(0, 40, 0), 0);
	var f = [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1500, 1600].atRandom;
	{
		RingzBank(
			s,
			{ f * Rand(1, 13) } ! p,
			1,
			{ Rand(0.4, 3.4) } ! p
		)
	} ! 2
}.overlap(8, 2, 4)
