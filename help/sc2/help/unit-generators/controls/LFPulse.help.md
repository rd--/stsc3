# LFPulse - pulse oscillator

_LFPulse(freq, phase, width)_

A non-band-limited pulse oscillator. Outputs a high value of one and a low value of zero.

- freq: frequency in Hertz
- width: pulse width duty cycle from zero to one.

Fixed frequency:

	LFPulse(500, 0, 0.3) * 0.1

Used as both Oscillator and LFO:

	LFPulse(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1

