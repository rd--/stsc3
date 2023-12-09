(* Latch *)
SinOsc(Latch(SinOsc([13.3, 13.7], 0), Impulse([9, 11], 0)) * 200 + 300, 0) * 0.1

(* Latch *)
Blip(Latch(WhiteNoise(), Impulse(9, 0)) * 400 + 500, 4) * 0.2

(* Latch *)
var l = Latch(WhiteNoise (), Impulse(9, 0));
SinOsc(l * 400 + 500, 0) * 0.2

(* ---- ; Latch plots *)
var d = Dust([1 5] * 100);
Latch(d, d)
