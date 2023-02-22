# scheduleInjecting -- scheduling

- _scheduleInjecting(aClock, deltaTime, aValue, aProcedure)_
- _scheduleInjecting(deltaTime, aValue, aProcedure)_ ⇒ aClock = workspace::clock
- _scheduleInjecting(aProcedure, aValue)_ ⇒ deltaTime = 0

Schedule applying _aProcedure_ to _aValue_ for _deltaTime_.
When evaluated the answer is a _[delay, nextValue]_ pair to continue, or _nil_ to halt.

Play ascending chromatic scale from C2 to C5, with random inter-offset delays and random durations:

```
{ :midiNoteNumber |
	{
		Release(
			EqPan2(
				SinOsc(midiNoteNumber.MidiCps, 0) * 0.1,
				Rand(-1, 1)
			),
			0.01, Rand(3, 7), 0.9
		)
	}.play;
	(midiNoteNumber <= 72).ifTrue {
		[
			[1/3, 1/2, 1, 3/2, 5].atRandom,
			midiNoteNumber + 1
		]
	}
}.scheduleInjecting(36)
```
