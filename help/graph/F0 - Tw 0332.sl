(* https://sccode.org/1-4Qy ; f0 ; 0332 *)
var f = Mul(
	2 ^ (SinOscFb([4, 3], 0) > 0),
	SinOscFb(1 / 16, 0) > 0 + 2 * (SinOscFb(1 / [32, 48], 0) > 0 * 20 + 99)
);
var o = SinOscFb(f, SinOscFb(1 / 63.9, 0) + 2 / 3);
AllpassN(o, 3, 3, 60) / 3
