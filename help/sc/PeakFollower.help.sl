# PeakFollower -- track peak signal amplitude

_PeakFollower(in, decay)_

Outputs the peak amplitude of the signal received at the input. If level is below maximum, the level decreases by the factor given in decay.

- in: input signal.
- decay: decay factor.

Internally, the absolute value of the signal is used, to prevent underreporting the peak value if there is a negative Dc offset. To obtain the minimum and maximum values of the signal as is, use the _RunningMin_ and _RunningMax_ UGens.

No decay:

	var peak = PeakFollower(Dust(20) * Ln(0, 1, 4), 1);
	SinOsc(peak * 1500 + 200, 0) * 0.1

A little decay:

	var peak = PeakFollower(Dust(20) * Ln(0, 1, 4), 0.999);
	SinOsc(peak * 1500 + 200, 0) * 0.1

Mouse controls decay:

	var decay = MouseX(0.99, 1.00001, 0, 0.1).Min(1);
	var peak = PeakFollower(Dust(20) * Ln(0, 1, 4), decay);
	SinOsc(peak * 1500 + 200, 0) * 0.1

Follow a sine lfo, decay controlled by mouse:

	var decay = MouseX(0, 1.1, 0, 0.1).Min(1);
	var peak = PeakFollower(SinOsc(0.2, 0), decay);
	SinOsc(peak * 200 + 500, 0) * 0.1

