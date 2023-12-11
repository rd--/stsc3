(* SvfBp ; changing F (slowly) *)
var f = XLine(20, 20000, 20);
SvfBp(WhiteNoise() * 0.1, f, 1)

(* SvfBp ; changing Q (slowly) *)
var q = Line(2, 0.0001, 20);
SvfBp(WhiteNoise() * 0.1, 400, q)

(* SvfBp ; modulating F *)
var f = SinOsc(XLine(0.7, 300, 20), 0) * 3600 + 4000;
SvfBp(Saw(200) * 0.1, f, 1)
