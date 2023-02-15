# SinOscBank -- sin oscillator bank

_SinOscBank(freqArray, ampArray, phaseArray)_

A summed set of fixed frequency sin oscillators.

- freqArray: frequency array
- ampArray: amplitude array
- phaseArray: phase array

Crossfading randomised oscillator banks:

```
{
	{
		SinOscBank(
			{ 600.Rand(1000) } ! 8,
			0.1,
			{ 1.Rand } ! 8
		)
	} ! 2 * 0.1
}.xfade(3, 4)
```

Equivalent to:

```
{
	{
		{
			FSinOsc(
				600.Rand(1000),
				1.Rand
			) * 0.1
		} !+ 8
	} ! 2 * 0.1
}.xfade(3, 4)
```

* * *

See also: _RingzBank_

