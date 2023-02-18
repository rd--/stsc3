# !+ -- duplicate and sum

- _anObject !+ anInteger_ ≡ (_anObject ! anInteger).sum_

Names the idiom of generating an array and then immediately summing it.
This operator does not need to construct the intermediate array structure to calculate the answer.

```
{
	{
		var f = (48 .. 72).atRandom.MidiCps;
		var ff = f * (SinOsc(ExpRand(4, 6), 0) * 0.008 + 1);
		LfSaw(
			[
				ff * Rand(0.99, 1.01),
				ff * Rand(0.99, 1.01)
			],
			0
		) * 0.01
	} !+ 10
}.overlap(2, 3, 4)
```
