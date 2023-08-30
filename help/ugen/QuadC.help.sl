(* QuadC ; default params *)
QuadC(SampleRate() / 4, 1, -1, -0.75, 0) * 0.2

(* QuadC ; logistic map ; equation: x1 = -r * x0 ^ 2 + r * x0 *)
var r = MouseX(3.5441, 4, 0, 0.2);
QuadC(SampleRate() / 4, r.Neg, r, 0, 0.1) * 0.4

(* QuadC ; logistic map as frequency control *)
var r = MouseX(3.5441, 4, 0, 0.2);
SinOsc(QuadC(40, r.Neg, r, 0, 0.1) * 800 + 900, 0) * 0.4

(* QuadC ; as simple oscillator *)
var f1 = MouseX(110, 880, 1, 0.2);
var f2 = MouseY(110, 440, 1, 0.2);
QuadC([f1, f2], 1, -1, -0.75, 0) * 0.1
