;; Resonz ; resonator
OverlapTexture({ :tr |
	Resonz(Pulse(TRand(2, 7, tr), 0.5), { TExpRand(120, 2500, tr) } ! 4, 0.005).Splay2
}, 2, 4, 6)

;; Resonz
Resonz(WhiteNoise() * 0.5, 2000, 0.1)

;; Resonz ; modulate frequency ; SinOsc
var f = SinOsc(0.5, 0) * 40 + 220;
Resonz(WhiteNoise(), f, 0.1)

;; Resonz ; modulate frequency ; XLn
var f = XLn(1000, 8000, 10);
Resonz(WhiteNoise() * 0.1, f, 0.05)

;; Resonz ; modulate bandwidth
var bw = XLn(1, 0.001, 8);
Resonz(WhiteNoise() * 0.1, 2000, bw)

;; Resonz ; modulate bandwidth opposite direction
var bw = XLn(0.001, 1, 8);
Resonz(WhiteNoise() * 0.1, 2000, bw)
