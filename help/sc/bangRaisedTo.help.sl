# !^ -- duplicate and distribute

- _anObject !^ anInteger_
- _p !^ q_ ⟹ bangRaisedTo(p, q)_

Names the idiom of generating an array and distributing it across the stereo spectrum.
_p !^ q_ is equivalent to _(p ! q).Splay2_.

```
{
	{
		var f = (48 .. 72).atRandom.MidiCps;
		var ff = f * (SinOsc(ExpRand(4, 6), 0) * 0.008 + 1);
		LfSaw(ff * Rand(0.99, 1.01), 0) * 0.05
	} !^ 10
}.overlap(2, 3, 4)
```
