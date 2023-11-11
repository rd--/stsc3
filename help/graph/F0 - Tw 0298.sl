(* https://sccode.org/1-4Qy ; f0 ; 0298 *)
var c =SinOscFb([2, 3], 0) % 1;
var f = XFade2(
	SinOscFb(1, 0).Ceiling * 36,
	SinOscFb(2, 0).RoundTo(1) * 2 + 4 * 12,
	SinOscFb(1 / [4, 12], 0),
	1
) + 8 * 4;
SinOscFb(f, c) * c * SinOscFb(1, 1 / 3)
