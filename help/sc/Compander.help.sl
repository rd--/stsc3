# Compander -- compressor, expander, limiter, gate, ducker

_Compander(in, control, thresh, slopeBelow, slopeAbove, clampTime, relaxTime)

General purpose (hard-knee) dynamics processor.

- in: The signal to be compressed / expanded / gated.

- control: The signal whose amplitude determines the gain applied to the input signal. Often the same as in (for standard gating or compression) but should be different for ducking.

- thresh: Control signal amplitude threshold, which determines the break point between slopeBelow and slopeAbove. Usually 0..1. The control signal amplitude is calculated using RMS.

- slopeBelow: Slope of the amplitude curve below the threshold. If this slope > 1.0, the amplitude will drop off more quickly the softer the control signal gets; when the control signal is close to 0 amplitude, the output should be exactly zero -- hence, noise gating. Values < 1.0 are possible, but it means that a very low-level control signal will cause the input signal to be amplified, which would raise the noise floor.

- slopeAbove: Same thing, but above the threshold. Values < 1.0 achieve compression (louder signals are attenuated); > 1.0, you get expansion (louder signals are made even louder). For 3:1 compression, you would use a value of 1/3 here.

- clampTime: The amount of time it takes for the amplitude adjustment to kick in fully. This is usually pretty small, not much more than 10 milliseconds (the default value). I often set it as low as 2 milliseconds (0.002).

- relaxTime: The amount of time for the amplitude adjustment to be released. Usually a bit longer than clampTime; if both times are too short, you can get some (possibly unwanted) artifacts.

If any of this is confusing, see <http://en.wikipedia.org/wiki/Audio_level_compression>

Example signal to process:

	Decay2(
		Impulse(8, 0) * MulAdd(LfSaw(0.3, 0), -0.3, 0.3),
		0.001,
		0.3
	) * Pulse([80, 81], 0.3).sum

Noise gate:

	var z = Decay2(
		Impulse(8, 0) * MulAdd(LfSaw(0.3, 0), -0.3, 0.3),
		0.001,
		0.3
	) * Pulse([80, 81], 0.3).sum;
	Compander(
		in: z,
		control: z,
		thresh: MouseX(0.1, 1, 0, 0.2),
		slopeBelow: 10,
		slopeAbove: 1,
		clampTime: 0.01,
		relaxTime: 0.01
	)

Compressor:

	var z = Decay2(
		Impulse(8, 0) * MulAdd(LfSaw(0.3, 0), -0.3, 0.3),
		0.001,
		0.3
	) * Pulse([80, 81], 0.3).sum;
	Compander(
		in: z,
		control: z,
		thresh: MouseX(0.1, 1, 0, 0.2),
		slopeBelow: 1,
		slopeAbove: 0.5,
		clampTime: 0.01,
		relaxTime: 0.01
	)

Limiter:

	var z = Decay2(
		Impulse(8, 0) * MulAdd(LfSaw(0.3, 0), -0.3, 0.3),
		0.001,
		0.3
	) * Pulse([80, 81], 0.3).sum;
	Compander(
		in: z,
		control: z,
		thresh: MouseX(0.1, 1, 0, 0.2),
		slopeBelow: 1,
		slopeAbove: 0.1,
		clampTime: 0.01,
		relaxTime: 0.01
	)

Sustainer:

	var z = Decay2(
		Impulse(8, 0) * MulAdd(LfSaw(0.3, 0), -0.3, 0.3),
		0.001,
		0.3
	) * Pulse([80, 81], 0.3).sum;
	Compander(
		in: z,
		control: z,
		thresh: MouseX(0.01, 0.15, 0, 0.2),
		slopeBelow: 1 / 3,
		slopeAbove: 1,
		clampTime: 0.01,
		relaxTime: 0.05
	) * 0.5