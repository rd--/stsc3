(* https://sccode.org/1-4Qy ; f0 ; 340 *)
var c = SinOsc([4, 3, 2] / 64, 0) % 1;
var p = Spring(Spring(Spring(SinOsc([2, 3], 0) > 0, 4, c[1] / 4), 9, c[2] / 3), 24, c[3] / 3) * 9;
CombN(SinOsc(0, p) * 0.7, 0.2, 0.2, 1)
