;; dark sea horns (jl)
var x = LocalIn(2, 0);
var a = (SinOsc(65, x * LfNoise1(0.1) * 3) * LfNoise1(3) * 6).tanh;
9.timesRepeat { a := AllpassL(a, 0.3, { 0.2.Rand + 0.1 } ! 2, 5) };
a := a.tanh;
(a * 0.1) <! LocalOut(a)
