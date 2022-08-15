# LFPulse - pulse oscillator

_LFPulse(freq, iphase,width)_

A non-band-limited pulse oscillator. Outputs a high value of one and a low value of zero.

- freq: frequency in Hertz
- iphase: initial phase offset in cycles (0..1)
- width: pulse width duty cycle from zero to one.

Mouse control of width:

	LFPulse(500, 0, MouseX(0, 1, 0, 0.2)) * 0.1

Used as both Oscillator and Lfo:

	LFPulse(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1

Compare with band limited Pulse UGen:

	[Pulse(100, 0.3), LFPulse(100, 0, 0.3)] * 0.15

