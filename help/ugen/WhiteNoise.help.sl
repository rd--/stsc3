(* WhiteNoise *)
WhiteNoise() * 0.05

(* WhiteNoise ; silence *)
var n = WhiteNoise() * 0.05;
n - n

(* WhiteNoise ; noise *)
var n = WhiteNoise () * 0.05;
var m = WhiteNoise () * 0.05;
n - m

(* WhiteNoise *)
var o = SinOsc(MouseY(900, 2300, 1, 0.2), 0);
var n = Lag(WhiteNoise(), MouseX(0.1, 1, 0, 0.2) * 0.01);
o * n * 0.1
