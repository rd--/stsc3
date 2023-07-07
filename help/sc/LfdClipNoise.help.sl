# LfdClipNoise -- Dynamic clipped noise

- _LfdClipNoise(freq=500)_

Generate the values -1 or +1 at a rate given by the _freq_ argument.

- freq: Approximate rate at which to generate random values.

There are two differences to LfClipNoise:

- no time quantization
- fast recovery from low freq values

If you dont need very high or very low freqs, or use fixed freqs, LfClipNoise is more efficient.

Mouse control of frequency. LfClipNoise will get stuck:

	LfClipNoise(MouseX(0.1, 1000, 1, 0.2)) * 0.1

LfdClipNoise will change smoothly:

	LfdClipNoise(MouseX(0.1, 1000, 1, 0.2)) * 0.1

LfClipNoise is silent for two seconds before going up in frequency:

	LfClipNoise(XLn(0.5, 10000, 3)) * 0.1

LfdClipNoise is more immediate:

	LfdClipNoise(XLn(0.5, 10000, 3)) * 0.1

LfClipNoise quantizes time steps at high frequencies

	LfClipNoise(XLn(1000, 20000, 10)) * 0.1

LfdClipNoise does not:

	LfdClipNoise(XLn(1000, 20000, 10)) * 0.1

* * *

See also: LfClipNoise, LfdNoise0, LfdNoise1, LfdNoise3, LfNoise0, LfNoise1, LfNoise2
