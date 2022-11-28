;; alien froggies (jmcc) #1
OverlapTexture({ :tr |
	var rate = 11;
	var r = Fold(rate * TRand(-0.2, [0.1, 0.2], tr).exp, 1, 30); (* bilinrand *)
	Formant(r, TExpRand([200, 300], 3000, tr), TRand([0, 1], 9, tr) * r + r) * 0.05;
}, 0.5, 0.25, 5)

;; alien froggies (jmcc) #1 ; left-to-right
var trRand = { :tr :lo :hi | TRand(lo, hi, tr) };
var trExpRand = { :tr :lo :hi | TExpRand(lo, hi, tr) };
OverlapTexture({ :tr |
	var r = tr.trRand(-0.2, [0.1, 0.2]).exp.Mul(11).Fold(1, 30);
	r.Formant(tr.trExpRand([200, 300], 3000), tr.trRand([0, 1], 9).MulAdd(r, r)).Mul(0.05);
}, 0.5, 0.25, 5)
