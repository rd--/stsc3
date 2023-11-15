(* https://twitter.com/luuma/status/656913884440301568 *)
Dfm1(
	Saw(11.arithmeticSeries(100, 0.9)).mean,
	PmOsc(100, 300, 0.4, 0).ExpRange(300, 9800),
	0.9, 1, 0, 0.0003
) ! 2 * AudioIn(1)
