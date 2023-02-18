# PulseCount -- pulse counter

_PulseCount(trig, reset)_

Each trigger increments a counter which is output as a signal.

- trig: trigger. Trigger can be any signal. A trigger happens when the signal changes from non-positive to positive.
- reset: resets the counter to zero when triggered.

Pulse count as frequency input:

	var count = PulseCount(Impulse(10, 0), Impulse(0.4, 0));
	SinOsc(count * 200, 0) * 0.05

