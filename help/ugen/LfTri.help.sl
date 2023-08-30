(* LfTri ; phase value = (0, 4), offset to lowest and midpoint ascending *)
LfTri(110, 4 * [0.75, 0]) * 0.1

(* LfTri ; as modulator *)
LfTri(MouseX(10, 60, 0, 0.2), 0.5) * SinOsc(780,0) * 0.05
