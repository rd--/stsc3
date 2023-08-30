(* % = floating point modulo *)
var f = LfCub(MouseY(0.25, [3, 5], 0, 0.2), 0) * 200 + 400 % MouseX([30, 70], 600, 0, 0.2);
SinOsc(f, 0) * 0.1
