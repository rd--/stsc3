# CuspL -- cusp map chaotic generator

_CuspL(freq, a, b, xi)_

- freq: iteration frequency in Hertz
- a, b: equation variables
- xi: initial value of x

A linear-interpolating sound generator based on the difference equation: _x(n + 1) = a - b * sqrt(|x(n)|)_

Vary frequency:

```
CuspL(
	MouseX(20, SampleRate(), 0, 0.2),
	1,
	1.99,
	0
) * 0.1
```

Mouse-controlled params:

```
CuspL(
	SampleRate() / 4,
	MouseX(0.9, 1.1, 1, 0.2),
	MouseY(1.8, 2, 1, 0.2),
	0
) * 0.1
````

As a frequency control:

```
SinOsc(
	CuspL(
		40,
		MouseX(0.9, 1.1, 1, 0.2),
		MouseY(1.8, 2, 1, 0.2),
		0
	) * 800 + 900,
	0
) * 0.2
