;; Latch
SinOsc(Latch(SinOsc(13.3, 0), Impulse(10, 0)) * 200 + 300, 0) * 0.2

;; Latch
Blip(Latch(WhiteNoise(), Impulse(9, 0)) * 400 + 500, 4) * 0.2

;; Latch
var l = Latch(WhiteNoise (), Impulse(9, 0));
SinOsc(l * 400 + 500, 0) * 0.2
