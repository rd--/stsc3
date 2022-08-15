# Impulse - impulse oscillator

_Impulse(freq, phase)_

Outputs non band limited single sample impulses.

- freq: frequency in Hertz
- phase: initial phase

Constant frequency:

	Impulse(800, 0) * 0.2

Modulate frequency:

	Impulse(XLn(800, 100, 5), 0) * 0.2

