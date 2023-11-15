(* https://sccode.org/1-4Qy ; f0 ; 0266 *)
var b = (1 .. 6) / 2;
var e = SinOsc(b / 99, 0) * 2 + 3;
var c = (SinOsc(b, 0) / (4 / b)) + e.RoundTo(1) * 99;
var d = SinOsc(c, 0).Splay;
d.Sin + (SinOsc(c * 1.5, e / d) * 0.7 / e).mean / 5
