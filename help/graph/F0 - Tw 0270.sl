(* https://sccode.org/1-4Qy ; f0 ; 0270 *)
var d = InFb(1, 0);
var f = 1 / [9, 8.9];
var c = Lag(Amplitude(d, 0.01, 0.01) < SinOscFb(f, 0), f / 9) + d;
AllpassN(SinOscFb(98.5 + c, d + f) * c, 1, f * 3, 20).Tanh
