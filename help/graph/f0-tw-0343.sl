;; https://sccode.org/1-4Qy ; f0 ; 0343
var b = 0.01 / (2 .. 6);
var c = SinOscFB(1 - b * 8, 1 / 2) + 1 / 2;
var e = Decay(c > 0.99512 * SinOscFB(c + 1 / b, 0), 1 / 2) / 22;
var o = SinOscFB(4 ** SinOscFB(b, 0) * 99, c);
Splay2((o * e).tanh)
