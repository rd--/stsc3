# XFade2 -- equal power two channel cross fade

_XFade2(inA, inB, pan, level)_

- inA & inB: input signals
- pan: at -1 hear only _inA_, at +1 hear only _inB_
- level: multiplier

Cross-fade between sawtooth and sine osillators.

	XFade2(Saw(440), SinOsc(440, 0), LfTri(0.1, 0), 0.1)
