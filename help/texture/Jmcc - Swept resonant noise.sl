(* swept resonant noise (jmcc) #2 *)
{
	var p = 10;(* number of partials *)
	var n = WhiteNoise() * 0.007;
	var f = (SinOsc(0.1 + 0.2.Rand, 0) * (12 + 12.Rand2) + 60 + 24.Rand2).MidiCps;
	var sweep = Resonz(n, f, 0.1);
	{
		RingzBank(
			sweep,
			{ 80 + LinRand(0, 10000, 0) } ! p,
			nil,
			{ 0.5 + 2.Rand } ! p
		)
	} ! 2
}.overlap(4, 4, 5)
