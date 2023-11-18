(* https://sccode.org/1-4Qy ; f0 ; 0004 *)
{ :currentTime |
	var z = 6 + 20.randomFloat;
	var d = 26 - z;
	{
		var y = LfTri(z, 0).Abs / 9 / z;
		var s = Rlpf(TDmdFor(y, 0, y), z * 600, 0.06);
		Release(EqPan(s, 0) * 4.5, d, 0, z)
	}.play;
	d
}.schedule
