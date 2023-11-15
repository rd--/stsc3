(* https://sccode.org/1-4Qy ; f0 ; 0355 *)
var b = (1 .. 9);
var o = LfSaw(LfSaw(9, 2 / b), 0) > 0.05 * GrayNoise() * 999 * LfSaw(440, 0);
MoogFf(o, 2 ^ LfSaw(b / 3.55, 0).RoundTo(1) * b * 99, 3.5, 0).Splay.Tanh / 9
