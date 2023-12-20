(* https://sccode.org/1-4Qy ; tweet0004 ; texture variant (rd) *)
{ :tr |
	var z = TRand(6, 26, tr);
	var y = LfTri(z, 0).Abs / 9 / z;
	Rlpf(TDmdFor(y, 0, y), z * 600, 0.06) * [9, 9]
}.OverlapTexture(6, 10, 2).Mix
