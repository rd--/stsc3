;; alien froggies (jmcc) #1
OverlapTexture({ :tr |
	var rate = 11;
	var r = Fold(rate * TRand(-0.2, [0.1, 0.2], tr).Exp, 1, 30); (* bilinrand *)
	Formant(r, TExpRand([200, 300], 3000, tr), TRand([0, 1], 9, tr) * r + r) * 0.05;
}, 0.5, 0.25, 5)

;; alien froggies (jmcc) #1 ; left-to-right
OverlapTexture({ :tr |
	var r = TRand(-0.2, [0.1, 0.2], tr).Exp.Mul(11).Fold(1, 30);
	r.Formant(TExpRand([200, 300], 3000, tr), TRand([0, 1], 9, tr).MulAdd(r, r)).Mul(0.05);
}, 0.5, 0.25, 5)
