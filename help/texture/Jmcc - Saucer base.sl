(* saucer base (jmcc) #6 *)
{
	var b = 1000.Rand;
	var c = 5000.Rand;
	var o1 = SinOsc(20.Rand, 0) * b + (1.1 * b);
	var o2 = SinOsc(o1, 0) * c + (1.1 * c);
	EqPan(SinOsc(o2, 0), 1.Rand2) * 0.1
}.overlap(6, 2, 4)
