(* DelayL *)
var osc = SinOsc(MouseX(110, 440, 1, 0.2), 0) * 0.1;
[osc, DelayL(osc, 4, MouseY(0.5, 4, 0, 2))]

(* DelayL ; stereo *)
var osc = SinOsc(MouseX(110, 440, 1, 0.2) * [1, 1.03], 0) * 0.1;
osc + DelayL(osc, 4, MouseY(0.5, 4, 0, 2))
