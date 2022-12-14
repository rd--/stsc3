# Cutoff -- very simple envelope shape

_Cutoff(sustain, decay, curve)_

Apply a cutoff envelope to a continuous sound.

- sustain: sustain portion of the envelope.
- decay: decay portion of the envelope.
- curve: shape of envelope segment.

Cutoff sine oscillator after four seconds with one second release time:

	Cutoff(4, 1, -4) * SinOsc(220, 0) * 0.1

