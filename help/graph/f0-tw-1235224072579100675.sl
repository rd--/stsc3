;; https://twitter.com/redFrik/status/1235224072579100675 ; ?
var f = 9;
var e = (f % pi).floor;
var b = [0 .. 8] - 1 + pi.tanh * pi * pi;
var c = 1 / b;
var j = SinOsc(b * f, c) * SinOsc(c, 0).max(0) + AllpassC(LocalIn(f, 0), e, SinOsc(c / f, 0) / f + e / e, e) * e;
var o = Bpf(Normalizer(j, e / e , c), LinExp(SinOsc(c, 0), -1, 1, f, e ** f * f), 1);
o.Splay2.sin <! LocalOut(o.rotateRight(1))