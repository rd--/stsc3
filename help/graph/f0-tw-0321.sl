;; https://sccode.org/1-4Qy ; f0 ; 0321
var b = [1 .. 6] * 60;
var c = SinOsc(b, LocalIn(6, 0) * 3);
var w = LocalOut(Limiter(Bpf(c, SinOsc(16 / b, 0) + 3 * b, 1), 0.66, 16 / b));
Splay2(c / 2) <! w