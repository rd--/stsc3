(* Linen ; note .kr only *)
var r = MouseX(1/3, 10, 0, 0.2);
SinOsc(440, 0) * Linen(Impulse(r, 0).kr, 0, 0.1, 1 / r, 0)
