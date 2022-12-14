# PulseDivider -- pulse divider

_PulseDivider(trig, div, startCount)_

Outputs one impulse each time it receives a certain number of triggers at its input.

- trig: trigger. Trigger can be any signal. A trigger happens when the signal changes from non-positive to positive.
- div: number of pulses to divide by.
- startCount: starting value for the trigger count. This lets you start somewhere in the middle of a count, or if startCount is negative it adds that many counts to the first time the output is triggers.

Lower tone at quarter the clock rate:

	var p = Impulse(8, 0);
	var a = SinOsc(1200, 0) * Decay2(p, 0.005, 0.1);
	var b = SinOsc(600,  0) * Decay2(PulseDivider(p, 4, 0), 0.005, 0.5);
	(a + b) * 0.4

Four divisions:

	var p = Impulse(8, 0);
	var a = SinOsc(1200, 0) * Decay2(p, 0.005, 0.1);
	var b = SinOsc(600,  0) * Decay2(PulseDivider(p, 4, 0), 0.005, 0.5);
	var c = SinOsc(800,  0) * Decay2(PulseDivider(p, 2, 1), 0.005, 0.5);
	var d = SinOsc(200,  0) * Decay2(PulseDivider(p, 16, 0), 0.005, 1.0);
	(a + b + c + d) * 0.3

