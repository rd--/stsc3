(* https://twitter.com/redFrik/status/1166129504747773954 *)
var z = (0 .. 2);
var f = z.collect { :i |
	i + 3.33 + LfTri(3 / 3333, 0)
} % 3 * 33 * (3 / (3 * pi) + LfPulse(0.003 * 3, 0, 0.5));
var p = LfTri(3 * 3 ^ [3.3, 3] + (LfTri([3, pi], 0)), 3);
var q = p * (LfPulse(pi, 0, 0.5) / 3 * LfTri(0.003, 0));
var r = ((LfTri(f, 3) * z.collect { :i |
	i + 3
}).product.Tanh * [3, 333 * LfTri(3 / 3333, 0), 33]).Splay2.Sin + q / 3;
r + z.collect { :i |
	PitchShift(r, 3 / 33, 3 / 3 + i * 3, 3 / 33, 3 / 33)
}.Mix / 9
