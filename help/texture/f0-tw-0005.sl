;; https://sccode.org/1-4Qy ; f0 ; 0005
{
	var z = 60.rand + 1;
	var d = z / 3;
	{
		var y = LfTri(z, 0).abs / z;
		Release(Pan2(Rlpf(TDmdFor(y, 0, y), z * 99 + y, 0.01) * (6 + y), 0, 1 / 3), d, 0, z)
	}.play;
	d
}.schedule