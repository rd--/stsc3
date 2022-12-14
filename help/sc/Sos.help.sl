# Sos -- second order filter section (biquad)

_Sos(in, a0, a1, a2, b1, b2)_

A standard second order filter section. Filter coefficients are given directly rather than calculated for you. Formula is equivalent to _out(i) = (a0 * in(i)) + (a1 * in(i-1)) + (a2 * in(i-2)) + (b1 * out(i-1)) + (b2 * out(i-2))_.

Same as TwoPole:

	var theta = MouseX(0.2 * pi, pi, 0, 0.2);
	var rho = MouseY(0.6, 0.99, 0, 0.2);
	var b1 = 2 * rho * theta.Cos;
	var b2 = rho.Squared.Neg;
	Sos(LfSaw(200, 0) * 0.1, 1, 0, 0, b1, b2)

