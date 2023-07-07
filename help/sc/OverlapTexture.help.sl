# OverlapTexture -- overlap events

_OverlapTexture(newEventFunc, sustainTime, transitionTime, overlap)_

Creates a series of overlapped sounds from a user function. The user function should return a graph of unit generators that produce a continuous sound. OverlapTexture will apply an envelope to the sound to cross fade different invocations of the user function.

- newEventFunc: You supply a function that returns a graph of unit generators. If it returns nil, then no event is spawned this time. This function is passed one argument, a trigger that is reset for each new event.
- sustainTime: the sustain time (in beats) of the envelope.
- transitionTime: the transition time (in beats) of the envelope. The envelope transition is a welch envelope segment giving it a -3dB midpoint.
- overlap: number of overlapping events.

Texture of overlapping sine tones:

```
{ :tr |
	{
		SinOsc(TRand(220, 990, tr), 0)
	} ! 2 * 0.1
}.OverlapTexture(3, 3, 3)
```

There are many examples of OverlapTexture in the examples files.

	var lfoFreq = 6;
	var lfo = LfNoise0(lfoFreq) * 1000 + 1200;
	var left = Rlpf(
		{ :tr |
			var f = TChoose(
				tr,
				[
					25, 30, 34, 37, 41, 42, 46,
					49, 53, 54, 58, 61, 63, 66
				]
			).MidiCps;
			[
				LfPulse(f, 0, 0.2),
				LfPulse(2 * f + TRand(-0.5, 0.5, tr), 0, 0.2)
			].sum
		}.OverlapTexture(4, 2, 4) * 0.02,
		lfo,
		MouseX(0.2, 0.02, 1, 0.2)
	);
	var delayTime = 2 / lfoFreq;
	var right = DelayC(left, delayTime, delayTime);
	[left, right] (* delay right channel by two beats *)

* * *

See also: _overlap_, _xfade_

