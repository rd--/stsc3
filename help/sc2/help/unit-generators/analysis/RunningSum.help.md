# RunningSum

_RunningSum(in, numsamp)_

A running sum over a user specified number of samples, useful for running RMS power windowing.

- in: Input signal
- numsamp: How many samples to take the running sum over (initialisation time only, not modulatable)

Overloads of course - would need scaling:

	RunningSum(AudioIn([1]), 40) / 40

Running Average over _x_ samples:

	var x = 100;
	RunningSum(LFSaw(440, 0), x) / x

Rms power:

	var input= LFSaw(440, 0);
	var numsamp = 30;
	(RunningSum(input.squared, numsamp) / numsamp).sqrt

Play around:

	var input = AudioIn([1]);
	var numsamp = 500;
	var power= MouseX(0.1, 4, 0, 0.2);
	(RunningSum(input ** power, numsamp) / numsamp) ** power.reciprocal

