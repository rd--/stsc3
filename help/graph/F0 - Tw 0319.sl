(* https://sccode.org/1-4Qy ; f0 ; 0319 *)
var b = (1 .. 8);
var c = SinOscFb(b / 16 / 16.16, 0) % 1;
var f = b.withIndexCollect { :x :i |
	[x, i + 6 / 6000 + x]
} * 60;
var e = (SinOscFb([3, 6], 1) * (Lag(c, 0.1) / 3)).Max(0);
var o = (SinOscFb(f, c) * e);
o.Splay2.transposed.Mix
