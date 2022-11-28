;; https://sccode.org/1-4Qy ; f0 ; 0281
var b = 2 / [2 .. 9];
var p = LfCub(b, 0) % LfCub(b / 5, 0);
var q = LfCub(2 ** LfCub(b / 8, 0) > 0 + 1 * 2 * (b * [300,303] - Lag2(LfCub(b / 9, 0) > 0 * 50, 0.1)), 0);
var r = LfCub(b / 6, b);
Splay2(p * q * r)
