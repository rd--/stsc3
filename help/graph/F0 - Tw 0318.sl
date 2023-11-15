(* https://sccode.org/1-4Qy ; f0 ; 0318 *)
var b = [1 .. 3];
MulAdd(
	SinOscFb(13 * 13 * b, 1 / 3),
	SinOscFb(b / 13, 1) / 13,
	SinOscFb(
		SinOscFb(1 / (13 .. 3), 0) + 133 * b,
		SinOscFb(b / 333, SinOscFb(b, 1) % 1) % 1
	)
).Splay / 3
