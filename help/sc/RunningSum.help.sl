# RunningSum -- summing

_RunningSum(in, numsamp)_

A running sum over a user specified number of samples, useful for running RMS power windowing.

- in: Input signal
- numsamp: How many samples to take the running sum over (initialisation time only, not modulatable)

Overloads of course, would need scaling:

	RunningSum(AudioIn([1]), 40) / 40

Running Average over _x_ samples:

	var x = 100;
	RunningSum(LfSaw(440, 0), x) / x

Rms power:

	var input= LfSaw(440, 0);
	var numsamp = 30;
	(RunningSum(input.Squared, numsamp) / numsamp).Sqrt

Play around:

	var input = AudioIn([1]);
	var numsamp = 500;
	var power= MouseX(0.1, 4, 0, 0.2);
	var sum = RunningSum(input ** power, numsamp);
	(sum / numsamp) ** power.Recip
