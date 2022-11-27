;; 20060920 (rd)
var x = MouseX(0.012, 0.19, 1, 0.1) + (LFNoise2(0.2) * 0.1 + 0.05);
var f = Formlet(Blip(10, 12), LFNoise0([20, 40]) * 43 + 700, 0.005, x);
var o = SinOsc(40, 0) * LFNoise0([5, 10]);
f + o * Ln(0, 0.25, 2.5)

;; 20060920 (rd)
var f = Formlet(
	Blip(10, 12),
	LFNoise0([20, 40]) * 43 + 700,
	0.005,
	MouseX(0.012, 0.19, 1, 0.1)
) + SinOsc(40, 0) * LFNoise0([5, 10]);
f * Ln(0, 0.25, 2.5)
