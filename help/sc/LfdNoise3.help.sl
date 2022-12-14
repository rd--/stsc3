# LfdNoise3 -- dynamic cubic noise

_LfdNoise3(freq)_

- freq: rate at which to generate random values.

Similar to _LfNoise2_, it generates polynomially interpolated random values at a rate given by the freq argument, with three differences:

- no time quantization
- fast recovery from low freq values
- cubic instead of quadratic interpolation

Use as frequency control:

```
SinOsc(
	LfdNoise3(4) * 400 + 450,
	0
) * 0.2
```
