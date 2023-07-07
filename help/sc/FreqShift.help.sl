# FreqShift -- Frequency Shifter

_FreqShift(input, shift, phase)_

FreqShift implements single sideband amplitude modulation, also known as frequency shifting, but not to be confused with pitch shifting. Frequency shifting moves all the components of a signal by a fixed amount but does not preserve the original harmonic relationships.

- input: audio input
- shift: amount of shift in cycles per second
- phase: phase of the frequency shift (0 - 2pi)

Shifting a 100Hz tone by 1 Hz rising to 500Hz:

	FreqShift(SinOsc(100, 0),XLn(1, 500, 5), 0) * 0.1

Shifting a complex tone by 1 Hz rising to 500Hz:

	FreqShift(
		SinOscBank([101, 303, 606, 808], 0.1, 0),
		XLn(1, 500, 10),
		0
	) * 0.1

Modulating shift and phase:

	FreqShift(
		SinOsc(10, 0),
		LfNoise2(0.3) * 1500,
		SinOsc(500, 0).LinLin(-1, 1, 0, 2 * pi)
	) * 0.1

Frequency shifting an audio sample:

	var sf = SfAcquire('floating_1', 1, [1]);
	FreqShift(
		PlayBuf(1, sf, 1, 0, 0, 1, 0),
		LfNoise0(0.45) * 1000,
		0
	) * 0.5

Shifting bandpassed noise:

	var snd = Bpf(WhiteNoise(), 1000, 0.001);
	[FreqShift(snd, LfNoise0(5.5) * 1000, 0) * 4, snd]

Simple detune & pitchmod via FreqShift:

	{
		var table = [0, 2, 4, 5, 7, 9, 11, 12];
		var octave = [0 .. 2].atRandom;
		var note = 48 + table.atRandom;
		var freq = (octave * 12 + note).MidiCps;
		var detune = 1.5;
		var osc = SinOsc(freq, 0) * 0.1;
		var left = osc + FreqShift(osc, freq * detune, 0);
		var right = FreqShift(left, SinOsc(3.23, 0) * 5, 0);
		[left, right] / 3
	}.overlap(3, 3, 3)

Shift pulse wave in opposite directions:

	{
		var table = [0, 2, 4, 5, 7, 9, 11, 12];
		var octave = [0 .. 2].atRandom;
		var note = 48 + table.atRandom;
		var freq = (octave * 12 + note).MidiCps;
		var width = SinOsc(2.3, 0).LinLin(-1, 1, 0.2, 0.8);
		var osc = Pulse(freq, width) * 0.1;
		var left = FreqShift(osc, XLn(-0.1, -200, 3), 0);
		var right = FreqShift(osc, XLn(0.1, 200, 3), 0);
		[left, right] / 3
	}.overlap(3, 3, 3)

FreqShift, feedback, FreqShift:

	{
		var table = [0, 2, 4, 5, 7, 9, 11, 12];
		var octave = [0 .. 2].atRandom;
		var note = 48 + table.atRandom;
		var freq = (octave * 12 + note).MidiCps;
		var in = FreqShift(
			InFb(1, 0) * 3.2,
			XLn(0.01, freq * 1.5, 1),
			0
		);
		var osc = SinOsc(freq, 0) * Sine(1, 9) * 0.1;
		var snd = FreqShift(
			osc + in,
			SinOsc(4.24, 0.5) * 3,
			0
		) * 0.5;
		(osc + snd) / 3 ! 2
	}.overlap(3, 3, 3)
