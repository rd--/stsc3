;; sweep ; modulate sine frequency
SinOsc(Sweep(Impulse(MouseX(0.5, 20, 1, 0.2), 0), 700) + 500, 0) * 0.1

;;---- sweep ; modulate sine frequency ; filter methods
MouseX(0.5, 20, 1, 0.2).impulse(0).sweep(700).add(500).sinOsc(0).mul(0.1)