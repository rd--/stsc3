(* https://sccode.org/1-4Qy ; f0 ; 0278 *)
var b = 1 / 2 ^ (2 .. 6);
var o = LfSaw(LfSaw(b, 0) > 0 * 3 + 15 / b, 0);
var f = pi ^ LfSaw(LfSaw(b, 0) + 1 * b, 0) * 999 + (LfSaw(LfSaw(b * 9, 0) * 9, 0) * 99) / 2;
Rlpf(o, f, 0.2).Splay / 5
