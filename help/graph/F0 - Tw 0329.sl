(* https://sccode.org/1-4Qy ; f0 ; 0329 *)
var e = SinOsc(97 / Lag((SinOsc(1 / 32, 0) + 2.2).RoundTo(1), 0.1), 0) * pi;
var c = SinOsc(99, LocalIn(9, 0) * SinOsc((1 .. 9) / 99 - 98, 0) * e);
c.Splay / 9 <! LocalOut(c)
