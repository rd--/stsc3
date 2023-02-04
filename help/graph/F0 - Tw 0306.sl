;; https://sccode.org/1-4Qy ; f0 ; 0306
var f = { :i |
	SinOsc(DmdFor(i + 1 / 9, 0, Dseq(inf, [1 .. 8].stutter(32)) * Dseq(inf, 8.fibonacciArray) * 99), 0)
};
Hpf(FreeVerb([0, 1].collect(f), 0.2, 1, 0.2), 9) / 3
