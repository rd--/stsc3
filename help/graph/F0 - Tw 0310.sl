(* https://sccode.org/1-4Qy ; f0 ; 0310 *)
var b = 1 / [3, 2];
var f = DmdFor(Trig(LocalIn(2, 0), SinOscFb(b, 0) + 11 / 2), 0, Dseq(inf, [1 .. 8])) * 99;
var c = SinOscFb(f, SinOscFb(b / 12, 0));
c <! LocalOut(c)
