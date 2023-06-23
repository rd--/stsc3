# LfdNoise0 -- dynamic step noise

_LfdNoise0(freq)_

- freq: rate at which to generate random values

Like LfNoise0, it generates random values at a rate given by the freq argument, with two differences:

- no time quantization
- fast recovery from low freq values

(The LfNoise Ugens quantize to the nearest integer division of the samplerate, and they poll the freq argument only when scheduled, and thus seem to hang when freqs get very low.)

If you don't need very high or very low freqs, or use fixed freqs, LfNoise0 is more efficient.

Try wiggling mouse quickly, LfNoise frequently seems stuck, LfdNoise changes smoothly.

	var x = MouseX(0.1, 1000, 1, 0.2);
	[LfNoise0(x), LfdNoise0(x)] * 0.1

Silent for two seconds before going up in freq.

	var f = XLn(0.5, 10000, 3);
	[LfNoise0(f), LfdNoise0(f)] * 0.1

LfNoise quantizes time steps at high freqs, LfdNoise does not:

	var f = XLn(1000, 20000, 10);
	[LfNoise0(f), LfdNoise0(f)] * 0.1
