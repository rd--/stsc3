(* https://sccode.org/1-4Qy ; f0 ; 0233 *)
var b = 1 / [1 4 6 8 11];
var c = LfTri(b / 98, 0);
var s = BufRd(1, (1 / b + 59).asLocalBuf, LfTri(b / 99, 0) + c * 5, 0, 1);
var q = Dseq(inf, s).MidiCps;
Splay(LfTri(DmdFor(b, c, q) + c, 0) / 2)
