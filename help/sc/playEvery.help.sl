# playEvery -- schedule sound generating procedure at intervals

- _playEvery(aClock, aProcedure, interval)_
- _playEvery(aProcedure, interval)_

Evaluate _aProcedure.play_ now, and re-schedule recursively after _interval.value_ seconds.
_aProcedure_ may accept either zero or one argument,
in the latter case the argument will be the delay time until it will next be invoked.

In the two-argument form scheduling is on _workspace::clock_.

The expression below schedules random sine tones at random intervals.

```
{
	Release(
		Pan2(
			SinOsc(IRand(48, 72).MidiCps, 0),
			Rand(-1, 1),
			Rand(0, 0.1)
		),
		Rand(0, 1),
		Rand(1, 2),
		Rand(2, 3)
	)
}.playEvery { 1 + 0.5.randomFloat }
```
