# Demand -- demand results from demand rate ugens

_Demand(trig, reset, demandUgens)_

When there is a trigger at the _trig_ input, a value is demanded each Ugen in the list and output.
The unit generators in the list should be _demand_ rate.

When there is a trigger at the reset input, the demand rate Ugens in the list are reset.

- trig: a trigger happens when the signal changes from non-positive to positive
- reset: resets the list of Ugens when triggered
- demandUgens: list of demand-rate Ugens to get values from. When the shortest stream ends, this Ugen will set the 'done' flag.

By design, a reset trigger only resets the demand ugens; it does not reset the value at Demand's output.
Demand continues to hold its value until the next value is demanded, at which point its output value will be the first expected item in the list.
To jump to the start value upon receipt of a reset trigger, one can add (+) the two triggers together:

One demand Ugen represents a single stream of values, so that embedding the same Ugen twice calls this stream twice.

Mouse control of tone:

```
var t = Impulse(24, 0);
var s = Drand(inf, [
	Dseq(1, [1, 2, 3, 4, 5, 4, 3, 2]),
	Drand(8, [4 .. 11])
]);
var f = Demand(t, 0, s * 100);
var x = MouseX(-1, 1, 0, 0.1);
var o = SinOsc([f, f + 0.7], 0);
o.Cubed.Cubed.ScaleNeg(x) * 0.1
```

* * *

See also: _DmdFor_, _Demand_, _Duty_
