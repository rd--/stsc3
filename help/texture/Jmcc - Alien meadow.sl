(* alien meadow ; jmcc ; #6 *)
{
	var b = Rand(0, 5000);
	var o1 = SinOsc(Rand(0, 20), 0) * (0.1 * b) + b;
	var o2 = SinOsc(Rand(0, 20), 0) * 0.05 + 0.05;
	EqPan(SinOsc(o1, 0) * o2, Rand(-1, 1))
}.overlap(6, 2, 6)
