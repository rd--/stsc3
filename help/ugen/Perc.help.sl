(* Perc ; EnvGen of EnvPerc without level input ; c.f. Asr for gate variant *)
var tr = Impulse(1, 0);
SinOsc(TrRand(tr, 200, 500), 0) * Perc(tr, 0.01, 1, -4) * 0.1

(* Perc *)
{
	var tr = Dust(1 / 4);
	SinOsc(TrRand(tr, 200, 500), 0) * Perc(tr, 0.01, TrRand(tr, 1, 4), -4) * 0.2
} !^ 9
