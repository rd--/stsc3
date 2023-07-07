# XFadeTexture -- crossfade events

_XFadeTexture(newEventFunc, sustainTime, transitionTime)_

Creates a series of overlapped sounds from a user function. The user function should return a graph of unit generators that produce a continuous sound. XFadeTexture will apply an envelope to the sound to cross fade different invocations of the user function.

- newEventFunc: You supply a function that returns a graph of unit generators. If it returns nil, then no event is spawned this time. This function is passed one argument, a trigger that is reset for each new event.
- sustainTime: the sustain time (in beats) of the envelope.
- transitionTime: the transition time (in beats) of the envelope. The envelope transition is a welch envelope segment giving it a -3dB midpoint.

There are many examples of XFadeTexture in the examples files.

A pair of sine oscillators crossfading between randomly shifting pitches and stereo locations.

```
XFadeTexture({ :tr |
	EqPan2(
		SinOsc(TiRand(48, 72, tr).MidiCps, 0),
		TRand(-1, 1, tr)
	) * 0.1
}, 2, 0.5)
```
