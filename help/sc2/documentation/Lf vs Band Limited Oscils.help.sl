# LfPulse vs. Pulse and LfSaw vs. Saw

In different situations you will want to use different versions of these unit generators.
The _Lf_ versions are faster but are not band limited.
That means that they can fold over the Nyquist frequency and cause aliased frequencies.
For example here is the same frequency sweep using both kinds of oscillators:

_LfPulse_ will alias at high frequencies.

	LfPulse(XLine(1000, 8000, 5), 0, 0.1) * 0.2

_Pulse_ is band limited and therefore smoother.

	Pulse(XLine(1000, 8000, 5), 0.1) * 0.2

Another pair of examples to compare, _Pulse_:

	Pulse(SinOsc(0.75, 0) * 5800 + 6000, 0.1) * 0.2

and _LfPulse_:

	LfPulse(SinOsc(0.75, 0) * 5800 + 6000, 0, 0.1) * 0.2

LfPulse is better to use as a low frequency controller because it is truly a rectangular wave,
plot the below to see the difference,
note also the opposite values for width:

	[LfPulse(100, 0, 0.3), Pulse(100, 0.7)] * 0.1

The same issues apply to _Saw_ and _LfSaw_.

LfSaw will alias at high frequencies.

	LfSaw(XLine(1000, 8000, 5), 0) * 0.2

Saw is band limited and therefore smoother.

	Saw(XLine(1000, 8000, 5)) * 0.2

Another pair of examples to compare, Saw:

	Saw(SinOsc(0.75, 0) * 5800 + 6000) * 0.2

and LfSaw:

	LfSaw(SinOsc(0.75, 0) * 5800 + 6000, 0) * 0.2

LfSaw is better to use as a low frequency controller because it is truly a sawtooth shape,
plot the below to see the difference,
note also phase offset for LfSaw,
and that it is ascending where Saw is descending,
and amplitude difference:

	[LfSaw(100, 1), Saw(100)] * 0.1
