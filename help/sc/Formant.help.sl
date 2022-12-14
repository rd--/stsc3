# Formant -- formant oscillator

_Formant(kfundfreq, kformfreq, kwidthfreq)_

Generates a set of harmonics around a formant frequency at a given fundamental frequency.

- kfundfreq: fundamental frequency in Hertz.
- kformfreq: formant frequency in Hertz.
- kwidthfreq: pulse width frequency in Hertz. Controls the bandwidth of the formant.

Widthfreq must be greater than or equal fundfreq.

Modulate fundamental frequency, formant freq stays constant:

	Formant(XLn(400, 1000, 8), 2000, 800) * 0.125

Modulate formant frequency, fundamental freq stays constant:

	Formant(200, XLn(400, 4000, 8), 200) * 0.125

Modulate width frequency, other freqs stay constant:

	Formant(400, 2000, XLn(800, 8000, 8)) * 0.125

