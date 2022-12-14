# Amplitude -- amplitude follower

_Amplitude(in, attackTime, releaseTime)_

Tracks the peak amplitude of a signal.

- in: input signal.
- attackTime: 60dB convergence time for following attacks.
- releaseTime: 60dB convergence time for following decays.

Use input amplitude to control Pulse amplitude - use headphones to prevent feedback.

	Pulse(90, 0.3) * Amplitude(AudioIn(1), 0.01, 0.01)

Use input amplitude to control SinOsc frequency - use headphones to prevent feedback.

	SinOsc(
		MulAdd(
			Amplitude(
				AudioIn(1),
				0.01,
				0.01
			),
			1200,
			400),
		0
	) * 0.2
