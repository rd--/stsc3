;; LinExp
var mod = SinOsc(Line(1, 10, 10, 0), 0);
[SinOsc(mod * 400 + 500, 0), SinOsc(LinExp(mod, -1, 1, 100, 900), 0)] * 0.1

;; LinExp
var s = LfSaw(0.25, 1);
SinOsc(LinExp(s + 1, 0, 2, 220, 440), 0) * 0.1

;; LinExp ; mce
var s = [LfSaw(1 / 4, 1), SinOsc(1 / 3, pi / 2)];
SinOsc(LinExp(s + 1, 0, 2, 220, 440), 0) * 0.1
