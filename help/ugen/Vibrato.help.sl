(* Vibrato ; note audio rate input *)
SinOsc(Vibrato(Dc(400), 1, 0.02, 0, 0, 0.04, 0.1, 0, 0), 0) * 0.1

(* Vibrato *)
var x = MouseX(2, 100, 0, 0.2);
var y = MouseY(0, 1, 0, 0.2);
var v = Vibrato(Dc(400), x, 0.1, 1, 1, y, 0.1, 0, 0);
SinOsc(v, 0) * 0.1

(* Vibrato *)
var n = LfNoise1(1) * 3 + 7;
var x = MouseX(0, 1, 0, 0.2);
var y = MouseY(0, 1, 0, 0.2);
var v = Vibrato(Dc(400), n, x, 1, 1, y, 0.1, 0, 0);
SinOsc(v, 0) * 0.1
