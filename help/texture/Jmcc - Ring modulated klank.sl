(* ring modulated klank ; jmcc ; #2 *)
{
	var p = 8; (* number of partials *)
	var i = Dust(20) * 0.02;
	var a = RingzBank(i, { Rand(100, 10000) } ! p, nil, { Rand(0.2, 1) } ! p);
	var o = SinOsc(LfNoise2(1.0 + 0.3.Rand2) * 200 + Rand(350, 400), 0) * a;
	EqPan(o, 1.Rand2)
}.overlap(4, 4, 4)
