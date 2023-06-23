# LfPulse -- pulse oscillator

_LfPulse(freq=440, phase=0, width=0.5)_

A non-band-limited pulse oscillator. Outputs a high value of one and a low value of zero.

- freq: frequency in Hertz
- iphase: initial phase offset in cycles (0..1)
- width: pulse width duty cycle from zero to one

Fixed frequency:

	LfPulse(500, 0, 0.3) * 0.1

Mouse control of width:

	LfPulse(500, 0, MouseX(0, 1, 0, 0.2)) * 0.1

Used as both Oscillator and Lfo:

	LfPulse(LfPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1

Compare with band limited Pulse UGen:

	[Pulse(100, 0.3), LfPulse(100, 0, 0.3)] * 0.15
