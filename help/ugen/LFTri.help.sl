;; LFTri ; phase value = (0, 4), offset to lowest and midpoint ascending
LFTri(110, 4 * [0.75, 0]) * 0.1

;; LFTri ; as modulator
LFTri(MouseX(10, 60, 0, 0.2), 0.5) * SinOsc(780,0) * 0.05
