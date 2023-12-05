(* https://sccode.org/1-4Qy ; tweet0016 ; ? *)
var b = BufAlloc(2, 90000).BufClear;
var i = Sweep(BufRd(2, b, Saw(12) * 30000 + 40000, 1, 2), 90000);
var w = BufWrite(b, i, 1, Saw([8, 9]));
BufRd(2, b <! w, i, 1, 2).transposed.Sum / 2
