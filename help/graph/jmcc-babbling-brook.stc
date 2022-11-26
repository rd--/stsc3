;; babbling brook (jmcc) #SC3
var b = { :f :m :a :g |
	RHPF(OnePole(BrownNoise(), 0.99), LPF(BrownNoise(), f) * m + a, 0.03) * g
};
{ b(14, 400, 500, 0.06) } ! 2 + { b(20, 800, 1000, 0.10) }.dup(2)

;; babbling brook (jmcc) #SC3 ; left-to-right
var b = { :f :m :a :g |
	BrownNoise().OnePole(0.99).RHPF(BrownNoise().LPF(f).MulAdd(m, a), 0.03).Mul(g)
};
{ b(14, 400, 500, 0.06) } ! 2 + { b(20, 800, 1000, 0.10) }.dup(2)
