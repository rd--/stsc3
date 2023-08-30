(* Tr1 ; c.f. Tr *)
[Tr1(SinOsc(10, 0)), Impulse(10, 0)] * 0.1

(* Tr1 *)
PinkNoise() * Decay2(Tr1(SinOsc([13, 19], 0)), 0.01, 0.1) * 0.1
