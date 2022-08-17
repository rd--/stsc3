# PitchShift

_PitchShift(in, windowSize, pitchRatio, pitchDispersion, timeDispersion)_

A time domain granular pitch shifter. Grains have a triangular amplitude envelope and an overlap of 4:1.

- in: the input signal.
- windowSize: the size of the grain window in seconds. This value cannot be modulated.
- pitchRatio: the ratio of the pitch shift. Must be from 0.0 to 4.0.
- pitchDispersion: the maximum random deviation of the pitch from the pitchRatio.
- timeDispersion: a random offset of from zero to timeDispersion seconds is added to the delay of each grain. Use of some dispersion can alleviate a hard comb filter effect due to uniform grain placement. It can also be an effect in itself. timeDispersion can be no larger than windowSize.

Modulate pitch ratio:

	var z = Blip(800, 6) * 0.1;
	PitchShift(z, 0.02, Ln(0.1, 4, 20), 0, 0.0001)

Pitch shift input.  **Use headphones** to prevent feedback:

	PitchShift(
		AudioIn([1, 2]), // stereo audio input
		0.1, // grain size
		MouseX(0, 2, 0, 0.2), // mouse x controls pitch shift ratio
		0, // pitch dispersion
		0.004 // time dispersion
	)

Use PitchShift to granulate input.  **Use headphones** to prevent feedback.  Upper left corner is normal playback. x = pitch dispersion, y = time dispersion.

	var grainSize = 0.5;
	PitchShift(
		AudioIn([1, 2]),
		grainSize,
		1, // nominal pitch rate = 1
		MouseX(0, 1, 0, 0.2), // pitch dispersion
		MouseY(0, grainSize, 0, 0.2) // time dispersion
	)