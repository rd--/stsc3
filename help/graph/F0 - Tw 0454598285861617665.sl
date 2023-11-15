(* https://twitter.com/redFrik/status/454598285861617665 *)
var b = (9 .. 1);
var c = LfTri(3 ^ LfTri(1 / b, b / 9), 0);
var d = LfTri(1 / b, 0) % 1 / 9 + 0.01;
var f = 2 ^ LfTri(b / 99, 0).RoundTo(1) * 99 * b;
var o = GrainSin(2, c, d, f, 0, -1, 512);
o.Tanh.Splay2.transposed.Mix / 2
