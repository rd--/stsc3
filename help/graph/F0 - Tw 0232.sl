;; https://sccode.org/1-4Qy ; f0 ; 0232 ; requires=pyramid
var b = [4, 3];
var q = 9.fib.pyramid(1) * 99;
var o = SinOsc(DmdFor(1 / b, 0, Seq(inf, q)), 0);
var e = SinOsc(b / 9, 0);
var d = SinOsc(b / 999, 0).Abs + 0.01;
CombC(o * e, 1.01, d, 9).Tanh / 2
