# LeakDc -- remove Dc

_LeakDc(in, coef)_

This filter removes a Dc offset from a signal.

- in: input signal.
- coef: leak coefficient.

Add Dc to a pulse wave and then remove it:

	var a = LfPulse(800, 0, 0.5) * 0.5 + 0.5;
	[a * 0.1, LeakDc(a, 0.995) * 0.1]

