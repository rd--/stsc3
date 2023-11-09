(* Clip ; https://scsynth.org/t/5208/2 (sp) *)
var freq = 10;
var osc = SinOsc(freq, 0);
var interp = (MouseX(1, 0, 0, 0.2) ^ 8) * 50 + 1;
var sig = Clip(osc * interp, -1, 1) * 0.5 + 0.5;
SinOsc(200 + (sig * 1000), 0) * 0.1
