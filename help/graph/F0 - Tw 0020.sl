(* tw 0020 (f0) *)
var a = { :freq |
	LfPulse(freq, 0, 0.5)
};
var b = (0 .. 3);
var a1 = a(b / 32) + 1 / 8;
var a2 = a(a1) + 1 * b;
var a3 = a(b / 64);
var a4 = a(a(b / 8)) * 2 + b;
var a5 = a(4 / (b + 1)); (* divide by zero *)
var a6 = a(a2);
var a7 = a(a6 + (a3.Sum + a5 * a4) * 100);
a7.Sum.EqPan2(0) * 0.1
