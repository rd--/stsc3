# Hasher -- randomized value

_Hasher(in)_

Returns a unique output value from zero to one for each input value according to a hash function.
The same input value will always produce the same output value.
The input need not be from zero to one.

- in: input signal

Hashing a straight line makes white noise:

```
Hasher(Ln(0, 1, 1)) * 0.2
```

Control rate hash of rounded mouse control, near values hash distinctly:

```
SinOsc(
	Hasher(
		MouseX(0, 10, 0, 0.2).RoundTo([0.2, 1]) + [0, 0.0001]
	) * 300 + 500,
	0
) * 0.1
```
