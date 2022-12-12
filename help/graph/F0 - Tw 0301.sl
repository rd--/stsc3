;; https://sccode.org/1-4Qy ; f0 ; 0301 ; requires=kr
var b = (0 .. 3).collect { :i |
	DmdFor(
		i + 1 / 9,
		0,
		ControlIn(1, i + 1 % 4) + Seq(inf, 8.fibonacciArray) % 9
	).kr
};
Splay2(CombN(SinOsc(b * 99, b * 2), 1, 1 / 3, 1) / 2) <! ControlOut(0, b)
