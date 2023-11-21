(* tw 0134 (f0) *)
var n = 50;
(1 .. n).collect { :i |
	var o1 = LfSaw(i + 1 / [3, 4], 0);
	var o2 = LfSaw(i + 1 / 8, 0) + 1;
	var f0 = o1 > o2 * (n / 2) + n;
	var m = LfSaw(i + 1 / n, i / (n / 2));
	var o3 = Blip(f0, i + [2, 3]) * m;
	Ringz(o3, i + 1 * (n * 2 - 1), 0.1)
}.mean / 5
