# OnePole -- one pole filter

_OnePole(in, coef)_

A one pole filter. Implements the formula _out(i) = ((1 - abs(coef)) * in(i)) + (coef * out(i-1))_.

- in: input signal to be processed
- coef: feedback coefficient. Should be between -1 and +1

Low pass:

	OnePole(WhiteNoise() * 0.2, 0.95)

High pass:

	OnePole(WhiteNoise() * 0.2, -0.95)

Modulate coeficient:

	OnePole(WhiteNoise() * 0.2, Ln(0.95, -0.95, 10))

