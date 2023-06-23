# Pulse -- band limited pulse wave

_Pulse(kfreq=440, kwidth=0.5)_

Band limited pulse wave generator with pulse width modulation.

- kfreq: frequency in Hertz
- kwidth: pulse width ratio from zero to one. 0.5 makes a square wave.

Modulate frequency:

	Pulse(XLn(40, 4000, 6), 0.1) * 0.1

Modulate pulse width:

	Pulse(200, Ln(0.01, 0.99, 8)) * 0.1

Two band limited square waves thru a resonant low pass filter:

	Rlpf(Pulse([100, 250], 0.5) * 0.1, XLn(8000, 400, 5), 0.05)

