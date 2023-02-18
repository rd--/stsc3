# Warp1 -- Warp a buffer with a time pointer

Inspired by Chad Kirby's SuperCollider2 Warp1 class, which was inspired by Richard Karpen's sndwarp for CSound. A granular time stretcher and pitchshifter.

_Warp1(numChannels, bufnum, pointer, freqScale, windowSize, envbufnum, overlaps, windowRandRatio, interp)_

- numChannels: number of channels at bufnum.
- bufnum: buffer number of soundfile.
- pointer: position in buffer, 0 is the beginning, 1 is the end.
- freqScale: amount of frequency shift, 1/2 is one octave down, 2 is one octave up, negative values play backwards.
- windowSize: size of each grain window.
- envbufnum: buffer number of grain envelope, -1 uses a built-in Hanning envelope.
- overlaps: number of overlapping windows.
- windowRandRatio: amount of randomness of the windowing function, 0 is no randomness, 1 is probably too random.
- interp: interpolation method, 1 = none, 2 = linear, 4 = cubic.

Here the pointer moves from the beginning to the end of the soundfile over fifteen seconds, control pitch with _MouseX_:

```
var sf = SfAcquire('floating_1', 1, [1]);
var pointer = LfSaw(1 / 15, 0).Range(0, 1);
var pitch = MouseX(0.5, 2, 0, 0.2);
Warp1(1, sf, pointer, pitch, 0.1, -1, 8, 0.1, 2) * 0.25
```

Pointer is _Phasor_, playback slows from unit to a quarter over twenty seconds:
​
```
var sf = SfAcquire('floating_1', 1, [1]);
var pointer = Phasor(
	0,
	SampleDur() / SfDur(sf) * XLn(1, 0.25, 20),
	0,
	1,
	0
);
var sound = Warp1(
	1,
	sf,
	pointer,
	1,
	0.3,
	-1,
	16,
	Ln(0, 1, 40),
	4
);
Pan2(sound, pointer * 2 - 1, 0.25).sum
```
