;; tw 0014 (f0) ; https://twitter.com/redFrik/status/24193534033
var f = { :i |
	var a0 = SinOscFB(i + 1, 1 / 9) * 999;
	var a1 = SinOscFB(1 / 9, 1) / 9;
	var a2 = SinOscFB(a0, 1 / 9) * a1;
	var a3 = SinOscFB(0.1, 3);
	var a4 = SinOscFB(a2, a3) * (i + 2 * 999);
	var a5 = SinOscFB(1 / 9, 1 / 9);
	SinOscFB(a4, a5) / 9
};
(1 .. 9).collect(f).sum ! 2 * 0.35
