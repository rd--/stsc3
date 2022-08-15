# FOS - first order filter section

_FOS(in, a0, a1, b1)_

A standard first order filter section. Filter coefficients are given directly rather than calculated for you.  Formula is equivalent to _out(i) = (a0 * in(i)) + (a1 * in(i-1)) + (b1 * out(i-1))_.

Same as OnePole:

	var x = MouseX(-1, 1, 0, 0.2);
	FOS(LFSaw(200, 0) * 0.1, 1 - x.abs, 0, x)

Same as OneZero:

	var x = MouseX(-1, 1, 0, 0.2);
	FOS(LFSaw(200, 0) * 0.1, 1 - x.abs, x, 0)

