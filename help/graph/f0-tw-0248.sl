;; https://sccode.org/1-4Qy ; f0 ; 0248
var d = LFTri(0.1, 0) < 0;
var b = d + [2 .. 8] / (d + 2);
var o = LFTri(b * 99, 0);
var r = Ringz(LFTri(b / 2, 0) > 0, b * 99, 5 ** LFTri(LFTri(0.01, 0) + 2 - b, 0) / 4);
Splay2(o + r).tanh
