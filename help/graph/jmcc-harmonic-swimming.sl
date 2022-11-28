;; harmonic swimming ; jmcc
var f = 50; (* fundamental frequency *)
var p = 20; (* number of partials per channel *)
var z = 0; (* start of oscil daisy chain *)
var offset = Ln(0, -0.02, 60); (* causes sound to separate and fade *)
(1 .. p).do { :i |
	z := MulAdd(
		FSinOsc(
			f * i, (* freq of partial *)
			0
		),
		MulAdd(
			LfNoise1(
				6 + [4.0.rand2, 4.0.rand2] (* amplitude rate *)
			),
			0.02, (* amplitude scale *)
			offset (* amplitude offset *)
		).max(0), (* clip negative amplitudes to zero *)
		z
	)
};
z

;; harmonic swimming (jmcc) #1
var a = 0.02;
var l = Line(0, 0 - a, 60, 0);
var o = { :h |
	var n = LfNoise1(6 + { Rand(-4, 4) } ! 2) * a + l;
	SinOsc(50 * (h + 1), 0) * n.max(0)
};
(0 .. 20).collect(o).sum
