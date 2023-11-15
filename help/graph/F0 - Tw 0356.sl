(* https://sccode.org/1-4Qy ; f0 ; 0356 ; requires=BHiPass4,BLowPass4 *)
var b = [4 16 3];
var o = LfTri(b, 0) * LfTri(b * b, 0);
var f1 = 8 ^ LfTri(b / 50, 0).RoundTo(1) * 99;
var f2 = 3 ^ LfTri(1 / b, 0) * 99;
BHiPass4(BLowPass4(o, f1, 1), f2, 0.1).Splay.Sin
