;; https://sccode.org/1-4Qy ; f0 ; 0318
var b = [1 .. 3];
var o1 = SinOscFb(13 * 13 * b, 1 / 3);
var o2 = SinOscFb(b / 13, 1);
var o3 = SinOscFb(1 / (13 .. 3), 0) + 133 * b;
var o4 = SinOscFb(b / 333, SinOscFb(b, 1) % 1) % 1;
Splay2(o1 * o2 / 13 + SinOscFb(o3, o4)) / 3
