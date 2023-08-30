(* sweep ; modulate sine frequency *)
SinOsc(Sweep(Impulse(MouseX(0.5, 20, 1, 0.2), 0), 700) + 500, 0) * 0.1

(* ---- sweep ; modulate sine frequency ; filter methods *)
MouseX(0.5, 20, 1, 0.2).Impulse(0).Sweep(700).Add(500).SinOsc(0).Mul(0.1)
