(* sc-140 ; 18 ; Nathaniel Virgo *)
var p = { PinkNoise() } ! 2;
var a = Brf(p + Blip(p + 2, 400), 150, 2) * 0.1;
var l = Lpf(p + 0.2 * Dust(0.1), 60);
var b = Lpf((FreeVerb2(l.first, l.second, 1, 1, 0.2) * 10000).Tanh, 2000);
a + b

