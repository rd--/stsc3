# XLn -- exponential line generator

_XLn(start, end, dur)_

Generates an exponential curve from the start value to the end value. Both the start and end values must be non-zero and have the same sign.

- start: starting value
- end: ending value
- dur: duration in seconds

Control frequency of sine oscillator:

	SinOsc(XLn(200, 17000, 10), 0) * 0.1

* * *

Note: This is the Sc _XLine_ Ugen without the _doneAction_ input.
