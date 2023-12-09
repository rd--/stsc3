(* https://sccode.org/1-4Qy ; tweet0006 ; texture variant (rd) *)
{ :tr |
	var n = MoogFf(
		ClipNoise() * 0.4,
		LfPar({ TRand(0, 0.3, tr) } ! 2, 0) * 600 + 990,
		2,
		0
	);
	GVerb(n, 9, 9, 1, 0.5, 15, 1, 0.7, 0.5, 300).transposed.Sum
}.OverlapTexture(1, 9, 6).Mix
