# LfBrownNoise1 -- noise generator

Random walk with Gendyn distributions.

_LfBrownNoise1(freq, dev, dist)_

- freq: approximate rate at which to generate random values
- dev: scale maximum step (0-1)
- dist: Gendyn distribution to use (0-5: see Gendy1 for details)

As audio signal:

```
LfBrownNoise1(1000, 1, MouseX(0, 5, 0, 0.2)) * 0.1
```

As frequency control:

```
SinOsc(
	LfBrownNoise1([8, 11], 0.2, 2) * 400 + 450,
	0
) * 0.1
```
