(* https://sccode.org/1-4Qy ; f0 ; 0336 *)
var d = LfSaw(1 / 9, 0);
var b = BufAlloc(1, 30000).BufClear;
var c = GrainBuf(2, d, 24, b, -2, 0, 2, 0, -1, 512);
var o = Bpf(Saw(d > 0 + 1 / 3 * 99) + c, 99, 4).mean;
c <! BufRec(b, 1, o)
