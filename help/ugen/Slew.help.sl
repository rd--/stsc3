(* Slew *)
var x = MouseX(200, 12000, 1, 0.2);
var y = MouseY(200, 12000, 1, 0.2);
[
	Slew (Saw(440).Neg, x, y) * 0.05,
	Slew(LfPulse(800, 0, 0.5), x, y) * 0.1
]

(* Slew ; square to triangle ; triangle to sin-like ; https://scsynth.org/t/5208/2 (sp) *)
var freq = 440;
var square = LfPulse(freq, 0, 0.5) * 2 - 1;
var tri = Slew(square, freq * 4, freq * 4);
var sin = LinLin(tri > 0, 0, 1, -1, 1) * (tri.Abs ^ (1 / 4));
XFade2(tri, sin, MouseX(-1, 1, 0, 0.2), 0.05)
