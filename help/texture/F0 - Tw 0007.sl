(* https://sccode.org/1-4Qy ; f0 ; 0007 *)
{
	var o = Pluck(
		Crackle([1.9 1.8]),
		Impulse(IRand(1, 6), 0),
		0.05,
		LinRand(0, 0.05, 0),
		1,
		0.5
	);
	Release(Bpf(o, 1200.IRand, 1), 9, 0, 69)
}.playEvery(9)
