(* https://sccode.org/1-4Qy ; f0 ; 0324 *)
var c = [1, 3, 5, 6];
var f = DmdFor(c + 1 / 16, 0, Dseq(inf, LfTri(1 / c / 8, 0) > 0 * 3 + c * 99));
Splay2(Hpf(BLowPass(LfTri(c, 0), f, 0.001) / 12, 9).Sin)
