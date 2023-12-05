(* https://www.youtube.com/watch?v=a6JAF-jHBlw ; lnu ; requires=Fm7 *)
var m = {
	LfTri(0.005.Rand, 0) * 0.0003 + 0.5
};
var v = LfTri(m(), 0) * 0.001 + 0.6;
var c = {
	Choose(
		CuspN(120 * m().Hypot(v.Tan) ^ v, 1, 1.9, 0),
		[0 5.1 -4.7 -12 7]
	)
};
var f = { :a |
	Fm7Matrix(
		[
			(c() + a).MidiCps,
			0,
			MulAdd(SinOsc(13 * v, 0), 0.2, (2 * v).Exp * v) / 8
		] ! 6 * v,
		m() ! 6 ! 6
	).SoftClip.Sum
};
Splay2(
	[0 3.1 7.05 10 -9 17].collect { :n |
		EqPan2(
			f(c() + 52 + n) * (12 * m().Neg).DbAmp,
			LfNoise2(500)
		)
	},
	0.5, 1, 0, true
).transposed.Mix
