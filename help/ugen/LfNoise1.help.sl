;; LfNoise1
(1 .. 5).collect({ :n |
	Pan2(SinOsc(n * 100, 0) * (LfNoise1(6) + Ln(1, -1, 30)).Max(0), Rand(-1, 1), 0.1)
}).sum * 0.25

;; LfNoise1
var freq = LinLin(LfNoise1(1), -1, 1, 220, 440);
SinOsc(freq, 0) * 0.1
