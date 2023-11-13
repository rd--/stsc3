(* Formant *)
var r = Fold(11 * Rand(-0.2, 0.2).Exp, 1, 30);
Formant(r, ExpRand(200, 3000), Rand(0, 9) * r + r) * 0.05

(* Formant ; requires=Voicer *)
Voicer(16) { :e |
	var f0 = [200 300 400 500] * e.x;
	var ff = LinExp(e.y, 0, 1, 400, 1200);
	Splay2(Formant(f0, ff, 200) * LagUd(e.w, 0.01, 1) * e.z)
}.Mix
