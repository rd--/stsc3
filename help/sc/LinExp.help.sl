# LinExp -- convert a linear range to an exponential range

_LinExp(in, srclo, srchi, dstlo, dsthi)_

Converts a linear range of values to an exponential range of values. The dstlo and dsthi arguments must be nonzero and have the same sign.

- in: input to convert.
- srclo: lower limit of input range.
- srchi: upper limit of input range.
- dstlo: lower limit of output range.
- dsthi: upper limit of output range.

Convert -1 to +1 sawtooth into 0.01 to 1.0 exponential:

	var s = LfSaw(500, 0);
	[s * 0.1, LinExp(s, -1, 1, 0.001, 0.1)]

Convert oscillator output to frequency input:

	var mod = SinOsc(Ln(1, 10, 10), 0);
	[
		SinOsc(mod * 400 + 500, 0),
		SinOsc(LinExp(mod, -1, 1, 100, 900), 0)
	] * 0.1

