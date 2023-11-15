(* https://sccode.org/1-4Qy ; f0 ; 0342 *)
var b = (1 .. 8) / 128;
var c = LfSaw(b, 0) % 1;
var f = (2 ^ LfSaw(b, 0) * 256).RoundTo(64);
var e = Rlpf(LfSaw(1 / b / 32, 0), 500, 1.01 - c);
var o = SinOscFb(f, c) * e;
Clip(o, 0, 1).Splay / 2
