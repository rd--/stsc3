(* tw 600429640600186880 (es) *)
var m = MouseX(0.925, 1, 0, 0.2);
var l = LocalIn(2, 0) * m + (Dust2([1, 1]) * 9);
var s = (2 .. 6).collect { :n |
	AllpassC(l, 0.6, LfNoise2(0.01) * 0.01 + (1 / n), 1)
}.Sum / 4.5;
l <! LocalOut(LeakDc(s, 0.995))

(* ---- ; notes.md
[status/600429640600186880](https://twitter.com/sluyterrific_sc/status/600429640600186880)
*)
