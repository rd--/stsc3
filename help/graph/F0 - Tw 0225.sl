(* tw 0225 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/617 *)
var b = (1 .. 8) * 99;
var o = Blip(b / 2 + (LfSaw(-8 / b, 1) * 99), b / 4 + (LfSaw(1 / b, 1) * 99));
CombN(o * SinOsc(8 / b, LfSaw(99 / b, 0)), 0.2, 0.2, 1).Splay.Sin * 0.5
