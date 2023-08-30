(* https://sccode.org/1-4Qy ; f0 ; 0295 *)
var o = SinOsc([600, 500, 99, 50, 8 / 3], 0) * SinOsc(0.1 / [9, 8, 7, 6, 5], 0);
var h = o.reduce { :i :j | i.Hypot(j) } / 2;
AllpassC(Hpf(h, 9), 1, SinOsc(1 / [80, 90], 0) / 3 + 0.5, 5)
