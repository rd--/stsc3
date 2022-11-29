;; ring modulated klank ; jmcc ; #2
{
	var p = 8; (* number of partials *)
	var i = Dust(20) * 0.02;
	var a = RingzBank(i, { 100.rrand(10000) } ! p, nil, { 0.2.rrand(1.0) } ! p);
	var o = SinOsc(LfNoise2(1.0 + 0.3.rand2) * 200 + 350 + 50.rand, 0) * a;
	Pan2(o, 1.0.rand2, 1)
}.overlap(4, 4, 4)