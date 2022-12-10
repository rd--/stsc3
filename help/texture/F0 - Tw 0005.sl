;; https://sccode.org/1-4Qy ; f0 ; 0005
{
	var z = Rand(1, 61);
	var d = z / 3;
	{
		var y = LfTri(z, 0).Abs / z;
		Release(Pan2(Rlpf(TDmdFor(y, 0, y), z * 99 + y, 0.01) * (6 + y), 0, 1 / 3), d, 0, z)
	}.play;
	d
}.schedule
