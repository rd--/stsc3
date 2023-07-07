# Gate -- gate or hold

_Gate(in, gate)_

Allows input signal value to pass when gate is positive, otherwise holds last value.

- in: input signal
- gate: trigger, occurs when the signal changes from non-positive to positive

Frequency is a random curve for 1/4 of a cycle and a held tone for 3/4 of a cycle:

```
var gatedNoise = Gate(
	LfNoise2(4),
	LfPulse(1.333, 0, 0.25)
);
SinOsc(gatedNoise * 100 + 200, 0) * 0.1
```
