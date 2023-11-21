(* tw 0014 (f0) ; https://twitter.com/redFrik/status/24193534033 *)
(1 .. 9).collect { :i |
	var a0 = SinOscFb(i + 1, 1 / 9) * 999;
	var a1 = SinOscFb(1 / 9, 1) / 9;
	var a2 = SinOscFb(a0, 1 / 9) * a1;
	var a3 = SinOscFb(0.1, 3);
	var a4 = SinOscFb(a2, a3) * (i + 2 * 999);
	var a5 = SinOscFb(1 / 9, 1 / 9);
	SinOscFb(a4, a5) / 9
}.Splay * 0.35
