;; https://sccode.org/1-4Qy ; f0 ; 0356 ; requires=BHiPass4 ; ?
var b = [4, 16, 3];
var o = LFTri(b, 0) * LFTri(b * b, 0);
var f1 = 8 ** LFTri(b / 50, 0).roundTo(1) * 99;
var f2 = 3 ** LFTri(1 / b, 0) * 99;
Splay2(BHiPass4(BLowPass4(o, f1, 1), f2, 0.1).sin)
