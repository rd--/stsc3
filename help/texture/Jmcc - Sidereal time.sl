(* sidereal time (jmcc) #9 *)
{
	var p = 15;
	var f = XLine(ExpRand(40, 300), ExpRand(40, 300), 12);
	var t = LfPulse(f, 0, Rand(0.1, 0.9)) * 0.002 * LfNoise2(8.Rand).Max(0);
	var z = { RingzBank(t, { ExpRand(100, 6000) } ! p, 0.1, { Rand(2, 6) } ! p).Distort } ! 2;
	CombN(z, 0.6, 0.5.Rand + 0.1, 8) + z.reversed
}.overlap(4, 4, 6)
