# Formlet -- FOF-like filter

_Formlet(in, freq, attackTime, decayTime)_

This is a resonant filter whose impulse response is like that of a sine wave with a Decay2 envelope over it. It is possible to control the attacktime and decaytime.

Formlet is equivalent to _Ringz(in, freq, decaytime) - Ringz(in, freq, attacktime)_.

Note that if _attacktime = decaytime_ then the signal cancels out and if _attacktime > decaytime_ then the impulse response is inverted.

The great advantage to this filter over FOF is that there is no limit to the number of overlapping grains since the grain is just the impulse response of the filter.

- in: input signal to be processed
- freq: resonant frequency in Hertz
- attackTime: 60 dB attack time in seconds.
- decayTime: 60 dB decay time in seconds.

Fixed parameters:

	Formlet(Impulse(20, 0) * 0.5, 1000, 0.01, 0.1)

Fixed parameters, modulate frequency of input signal:

	Formlet(Blip(XLn(10, 400, 8), 1000) * 0.1, 1000, 0.01, 0.1)

Modulating formant frequency:

	var input = Blip(SinOsc(5, 0) * 20 + 300, 1000) * 0.1;
	Formlet(input, XLn(1500, 700, 8), 0.005, 0.04)

Mouse control of frequency and decay time:

```
Formlet(
	Blip(SinOsc(5, 0) * 20 + 300, 1000) * 0.1,
	MouseY(700, 2000, 1, 0.2),
	0.005,
	MouseX(0.01, 0.2, 1, 0.2)
)
```

