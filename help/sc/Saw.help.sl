# Saw -- band limited sawtooth

_Saw(kfreq)_

Band limited sawtooth wave generator.

- kfreq: frequency in Hertz

Modulating the frequency:

	Saw(XLn(40, 4000, 6)) * 0.1

Two band limited sawtooth waves through a resonant low pass filter:

	Rlpf(Saw([100, 250]) * 0.1, XLn(8000, 400, 5), 0.05)

