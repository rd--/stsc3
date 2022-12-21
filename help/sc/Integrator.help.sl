# Integrator -- leaky integrator

_Integrator(in, leak)_

Integrates an input signal with a leak. The formula implemented is: _out(0) = in(0) + (leak * out(-1))_

- in: input signal
- leak: leak coefficient.

As filter:

	Integrator(LfPulse(300, 0, 0.2) * 0.1, 0.9)

Used as an envelope:

	Integrator(LfPulse(3, 0, 0.2) * 0.0004, 0.999) * FSinOsc(700, 0)
