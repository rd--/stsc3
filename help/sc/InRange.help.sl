# InRange -- tests if a signal is within a given range

_InRange(in, lo, hi)_

If in is >= lo and <= hi output 1, otherwise output 0. Output is initially zero.

- in: signal to be tested
- lo: low threshold
- hi: high threshold

Trigger noise burst:

```
InRange(
	SinOsc(1, 0) * 0.2,
	[-0.15, -0.1],
	[0.15, 0.2]
) * PinkNoise() * 0.1
```
