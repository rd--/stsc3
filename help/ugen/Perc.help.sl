;; Perc ; EnvGen of EnvPerc without level input ; c.f. Asr for gate variant
var t = Impulse(1, 0);
SinOsc(TRand(200, 500, t), 0) * Perc(t, 0.01, 1, -4) * 0.1

;; Perc
{
	var t = Dust(1 / 4);
	SinOsc(TRand(200, 500, t), 0) * Perc(t, 0.01, TRand(1, 4, t), -4) * 0.2
} !^ 9
