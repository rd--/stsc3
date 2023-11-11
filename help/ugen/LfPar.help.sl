(* LfPar ; phase value = (0, 4), offset to lowest and midpoint ascending *)
LfPar(110, 4 * [0.5, 0.75]) * 0.1

(* LfPar ; as nested frequency modulator *)
LfPar(LfPar(LfPar(0.2, 0) * 8 + 10, 0) * 400 + 800, 0) * 0.1

(* LfPar ; as frequency modulator *)
LfPar(LfPar(0.2, 0) * 400 + 800, 0) * 0.1

(* LfPar ; frequency sweep *)
LfPar(XLine(100, 8000, 30), 0) * 0.1
