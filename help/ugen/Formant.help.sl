(* Formant *)
var r = Fold(11 * Rand(-0.2, 0.2).Exp, 1, 30);
Formant(r, ExpRand(200, 3000), Rand(0, 9) * r + r) * 0.05
