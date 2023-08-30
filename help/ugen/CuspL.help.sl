(* CuspL ; frequency control *)
var x = MouseX(0.9, 1.1, 0, 0.1);
var y = MouseY(1.8, 2.0, 0, 0.1);
var n = CuspL([30, 40], x, y, 0) * 0.3;
SinOsc(n * [700, 800] + 900, 0) * 0.1
