# collectTexture -- scheduling

- _collectTexture(aCollection, aProcedure, aDelay)_

Evaluate _aProcedure_ at each element of _aCollection_ with _aDelay.value_ seconds between each occurence, and _play_ each answer.

Play ascending diatonic scale:

```
[0, 2, 4, 5, 7, 9, 11, 12].collectTexture { :pitchClass |
	Release(
		EqPan2(
			SinOsc(MidiCps(pitchClass + 48), 0) * 0.1,
			Rand(-1, 1)
		),
		3, 4, 3
	)
} { [1, 3, 5, 7].atRandom }
```
