;; Blip
var ln = Ln(1, 20, 60);
var gate = LfPulse(ln, 0, 0.23) > 0;
Blip(TRand(100, 1000, gate), TRand(1, 10, gate)) * Asr(gate, 0.01, 1 / ln, -4) * 0.25

;; Blip
var ln = Ln(1, 20, 60);
var tr = Impulse(ln, 0);
Blip(TRand(100, 1000, tr), TRand(1, 10, tr)) * Perc(tr, 0.01, 1 / ln, -4) * 0.25

;; Blip
Blip(
	freq: MouseX([3, 12], 1, 0, 0.2),
	numharm: MouseY([100, 1000], 1, 0, 0.2)
) * 0.1