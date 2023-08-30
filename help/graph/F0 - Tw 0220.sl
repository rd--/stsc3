(* tw 0220 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/617 *)
var c = InFb(1, 0);
var b = BufAlloc(1, 90000).BufClear;
var g = TGrains(2, SinOsc(3, 0), b, c + 3, 2, 12, 0, 0.1, 4);
var r = RecordBuf(b, 0, 1, 0, 1, 1, 1, 0, c);
var o = Hpf(SinOsc(99, c * 6) / 9 + g, 9) * 0.25;
o <! r
