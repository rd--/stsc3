# LeakDC - remove DC

_LeakDC(in, coef)_

This filter removes a DC offset from a signal.

- in: input signal.
- coef: leak coefficient.

Add DC to a pulse wave and then remove it:

	var a = LFPulse(800, 0, 0.5) * 0.5 + 0.5;
	[a * 0.1, LeakDC(a, 0.995) * 0.1]

