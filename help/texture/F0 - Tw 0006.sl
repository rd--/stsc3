(* https://sccode.org/1-4Qy ; f0 ; 0006 *)
{
	var n = MoogFf(
		ClipNoise() * 0.4,
		LfPar({ 0.3.Rand } ! 2, 0) * 600 + 990,
		2,
		0
	);
	var s = GVerb(n, 9, 9, 1, 0.5, 15, 1, 0.7, 0.5, 300).transposed.Mix;
	Release(s * 0.1, 3, 0, 19)
}.playEvery(3)
