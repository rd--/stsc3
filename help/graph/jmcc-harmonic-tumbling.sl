;; harmonic tumbling ; jmcc
var f = 80; (* fundamental frequency *)
var p = 10; (* number of partials per channel *)
var z = 0.0; (* start of oscil daisy chain *)
var trig = XLn(10 ! 2, 0.1, 60); (* trigger probability decreases over time *)
(1 .. p).do { :i |
	z := MulAdd(
		FSinOsc(
			f * i, (* freq of partial *)
			0
		),
		Decay2(
			Dust(
				trig (* trigger rate *)
			) * 0.02, (* trigger amplitude *)
			0.005, (* grain attack time *)
			0.5.rand (* grain decay time *)
		),
		z
	)
};
z

;; harmonic tumbling (jmcc) #1
var t = XLine([10, 11], 0.1, 60, 0);
var o = { :h |
	var e = Decay2(Dust(t) * 0.02, 0.005, Rand(0, 0.5));
	SinOsc(80 * (h + 1), 0) * e
};
(0 .. 10).collect(o).sum