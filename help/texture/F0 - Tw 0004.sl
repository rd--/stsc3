;; https://sccode.org/1-4Qy ; f0 ; 0004
{
	var z = 6 + 20.randomFloat;
	var d = 26 - z;
	{
		var y = LfTri(z, 0).abs / 9 / z;
		Release(Pan2(Rlpf(TDmdFor(y, 0, y), z * 600, 0.06), 0, 9) * 0.5, d, 0, z)
	}.play;
	d
}.schedule
