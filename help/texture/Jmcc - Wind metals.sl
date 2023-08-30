(* wind metals (jmcc) *)
{
	var n = 6;
	var env = (LfNoise1(ExpRand(0.125, 0.5)) * 0.75 + 0.25).Max(0);
	var exc = { BrownNoise() } ! 2 * 0.007 * env;
	var freq = { Rand(500, 8000).Rand + ExpRand(60, 4000) } ! n;
	var time = { Rand(0.1, 2) } ! n;
	var s = RingzBank(exc, freq, nil, time);
	(s * 0.1).SoftClip
}.overlap(5, 2, 12)
