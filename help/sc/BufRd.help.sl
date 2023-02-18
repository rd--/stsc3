# BufRd -- Buffer reading oscillator

Read the content of a buffer at an index.

Where PlayBuf plays through the buffer by itself, BufRd only moves its read point by the phase input and therefore has no pitch input. BufRd has variable interpolation.

_BufRd(numChannels, bufnum=0, phase=0, loop=1, interpolation=2)_

- numChannels: number of channels
- bufnum: signal buffer index
- phase: index into the buffer
- loop: 1 means true, 0 means false
- interpolation: 1 means no interpolation, 2 is linear, 4 is cubic interpolation.

Zig zag around sound:

	var sf = SfAcquire('floating_1', 1, [1]).first;
	var phase = LfNoise2(MouseX(2, 20, 1, 0.2)) * SfFrames(sf);
	BufRd(1, sf, phase, 1, 2)

Ordinary playback, phase courtesy _LfSaw_:

	var sf = SfAcquire('floating_1', 1, [1]).first;
	var sw = LfSaw(SfDur(sf).Recip, 0);
	var ph = LinLin(sw, -1, 1, 0, SfFrames(sf));
	BufRd(1, sf, ph, 1, 2)

Ordinary playback, phase courtesy _Phasor_:

	var sf = SfAcquire('floating_1', 1, [1]).first;
	var ph = Phasor(0, SfRateScale(sf), 0, SfFrames(sf), 0);
	BufRd(1, sf, ph, 1, 2)

