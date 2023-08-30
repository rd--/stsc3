(* Tr ; c.f. Tr1 *)
[Tr(SinOsc(10, 0)) > 0, Impulse(10, 0)] * 0.1

(* Tr *)
PinkNoise() * Decay2(Tr(SinOsc([13, 19], 0)) > 0, 0.01, 0.1) * 0.1
