(* coolant (jmcc) #2 *)
{
	var p = 10;
	var exc = OnePole(BrownNoise() * 0.002, 0.95);
	{ RingzBank(exc, { 40 + 2000.Rand } ! p, 1, 1) } ! 2
}.overlap(4, 4, 2)
