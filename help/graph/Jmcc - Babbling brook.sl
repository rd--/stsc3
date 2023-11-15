(* babbling brook (jmcc) #Sc3 *)
var b = { :f :m :a :g |
	Rhpf(
		OnePole(BrownNoise(), 0.99),
		Lpf(BrownNoise(), f) * m + a,
		0.03
	) * g
};
[
	{ b(14, 400, 500, 0.06) } ! 2,
	{ b(20, 800, 1000, 0.10) } ! 2
].Mix
