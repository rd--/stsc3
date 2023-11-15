(* https://twitter.com/redFrik/status/1467507942664646661 ; requires=FbSineC *)
var b = [3 4 5 6 7 8 1 2];
var fb1 = { :freq | FbSineC(freq, 1, 0.1, 1.1, 0.5, 0.1, 0.1) };
var d = fb1(b / 12) > 0 / 80 + 1.051;
var c = fb1(1 / b) + 1;
var p = FbSineC(
	999 * b,
	2 ^ fb1(b / 9) * 4 - 1,
	fb1(b / 8) / 2 + 1 + (99 ^ fb1(1 / 4) / 99),
	d,
	fb1(1 / b) / 40 + 0.1,
	0.1,
	0.1
);
var q = FbSineC(
	fb1(27 + b) > 0 + 2 + b * d * 1400,
	2,
	c,
	c / 2,
	2,
	2,
	0.1
);
(3 ^ fb1(1 / b) / 5 * p + (q / 12) + (Hpf(fb1(32) < 0, 3) * fb1(b * 70 / d) * fb1(1 / 2))).Splay / 2
