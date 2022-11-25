# PeakFollower - track peak signal amplitude

_PeakFollower(in, decay)_

Outputs the peak amplitude of the signal received at the input.  if level is below maximum, the level decreases by the factor given in decay.

- in: input signal.
- decay: decay factor.

Internally, the absolute value of the signal is used, to prevent underreporting the peak value if there is a negative DC offset. To obtain the minimum and maximum values of the signal as is, use the _RunningMin_ and _RunningMax_ UGens.

No decay:

	SinOsc(PeakFollower(Dust(20) * Ln(0, 1, 4), 1) * 1500 + 200, 0) * 0.1

A little decay:

	SinOsc(PeakFollower(Dust(20) * Ln(0, 1, 4), 0.999) * 1500 + 200, 0) * 0.1

Mouse controls decay:

	var decay = MouseX(0.99, 1.00001, 0, 0.1).min(1);
	SinOsc(PeakFollower(Dust(20) * Ln(0, 1, 4), decay) * 1500 + 200, 0) * 0.1

Follow a sine lfo, decay controlled by mouse:

	var decay = MouseX(0, 1.1, 0, 0.1).min(1);
	SinOsc(PeakFollower(SinOsc(0.2, 0), decay) * 200 + 500, 0) * 0.1

