(* tw 0084 (f0) ; http://sccode.org/1-4Qy *)
var f = Saw(0...5 * 2 + 5 * 19) * 99 + 199;
var g = Saw(0...6 * 2 + 1 * 29) * 199 + 299;
var w = Saw(0...4 * 2 + 3 * (Saw(3) * 2 + 3)) * 299 + 399;
Formant(f, g, w).Splay2 / 3 / 3
