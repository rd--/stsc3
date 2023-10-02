(* https://sccode.org/1-4Qy ; f0 ; 0346 *)
var c = SinOscFb(1 - SinOscFb(1 / [3, 2], 0).RoundTo(0.5) + InFb(2, 0), 0);
(SinOscFb(2 ^ SinOscFb(1 - c, 0).RoundTo(1 - c) * 400, c.Abs) * Lag(c, 0.1) * c).Tanh
