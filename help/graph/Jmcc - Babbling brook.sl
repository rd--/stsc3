;; babbling brook (jmcc) #SC3
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
].sum

;; babbling brook (jmcc) #SC3 ; left-to-right
var b = { :f :m :a :g |
	BrownNoise()
		.OnePole(0.99)
		.Rhpf(BrownNoise()
			.Lpf(f)
			.MulAdd(m, a), 0.03)
		.Mul(g)
};
[
	{ b(14, 400, 500, 0.06) } ! 2,
	{ b(20, 800, 1000, 0.10) } ! 2
].sum
