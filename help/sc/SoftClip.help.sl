# softclip -- nonlinear distortion

Distortion with a perfectly linear region from -0.5 to +0.5.

	(FSinOsc(500, 0) * XLn(0.1, 10, 10)).softclip * 0.1

