(* ControlIn ; read from control bus *)
var writer = ControlOut(7, LfNoise2(1) * 0.5 + 0.5);
var reader = SinOsc(ControlIn(1, 7) * 100 + 200, 0) * 0.1;
reader <! writer

(* ControlIn ; read from control bus ; stereo *)
var writer = ControlOut(7, LfNoise2([1, 3]) * 0.5 + 0.5);
var reader = SinOsc(ControlIn(2, 7) * 100 + 200, 0) * 0.1;
reader <! writer
