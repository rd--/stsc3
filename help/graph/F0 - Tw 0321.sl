(* https://sccode.org/1-4Qy ; f0 ; 0321 *)
var b = (1 .. 6) * 60;
var c = SinOsc(b, LocalIn(6, 0) * 3);
var d = Bpf(c, SinOsc(16 / b, 0) + 3 * b, 1);
var w = LocalOut(Limiter(d, 0.66, 16 / b));
c.Splay / 2 <! w
