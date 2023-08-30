(* alien meadow ; jmcc ; #6 *)
{
	var b = 5000.Rand;
	var o1 = SinOsc(20.Rand, 0) * (0.1 * b) + b;
	var o2 = SinOsc(20.Rand, 0) * 0.05 + 0.05;
	EqPan2(SinOsc(o1, 0) * o2, 1.Rand2)
}.overlap(6, 2, 6)
