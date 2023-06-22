# Impulse -- impulse oscillator

_Impulse(freq=440, phase=0)_

Outputs non band limited single sample impulses.

- freq: frequency in Hertz
- phase: phase offset in cycles (0..1)

Constant frequency:

	Impulse(800, 0) * 0.2

Modulate frequency:

	Impulse(XLn(800, 100, 5), 0) * 0.2

Modulate phase:

	Impulse(4, [0, MouseX(0, 1, 0, 0.2)]) * 0.2
