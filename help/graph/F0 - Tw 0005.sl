(* https://sccode.org/1-4Qy ; tweet0005 ; texture variant (rd) *)
{ :tr |
	var z = TrRand(tr, 1, 60);
	var y = LfTri(z, 0).Abs / z;
	{
		Rlpf(TDmdFor(y, 0, y), z * 99 + y, 0.01) * (6 + y)
	} ! 2
}.OverlapTexture(1, 25, 4)
