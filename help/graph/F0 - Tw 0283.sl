(* https://sccode.org/1-4Qy ; f0 ; 0283 *)
var b = 1 / (2 .. 6);
var o1 = SinOscFb(SinOscFb(b, 1) < b * 500 + 99, 0) / 5;
var o2 = SinOscFb(999 * b, SinOscFb(SinOscFb(b, 1) < 0.1 + 1, 1) % b);
var o3 = SinOscFb(0.1 - b, 1).Min(0);
Splay(o1 + (o2 * o3)) / 2
