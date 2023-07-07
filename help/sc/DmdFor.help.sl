# DmdFor -- demand results from demand rate ugens

_DmdFor(dur, reset, level)_

A value is demanded of each Ugen in the list and output according to a stream of duration values.
The unit generators in the list should be _demand_ rate.

When there is a trigger at the reset input, the demand rate Ugens in the list and the duration are reset.
The reset input may also be a demand Ugen, providing a stream of reset times.

- dur: time values, the next level is acquired after duration
- reset: resets the list of Ugens and the duration Ugen when triggered.
- level: demand Ugen providing the output values

The reset input may also be a demand UGen, providing a stream of reset times.

Demand Ugen as durations:

```
var freq = DmdFor(
	Drand(inf, [0.01, 0.2, 0.4]),
	0,
	Dseq(inf, [204, 400, 201, 502, 300, 200])
);
SinOsc(freq * [1, 1.01], 0) * 0.1
```

Control rate ugen as durations:

```
var freq = DmdFor(
	MouseX(0.001, 2, 1, 0.2),
	0,
	Dseq(inf, [204, 400, 201, 502, 300, 200])
);
SinOsc(freq * [1, 1.01], 0) * 0.1
```

Control rate resetting the demand ugens:

```
var freq = DmdFor(
	Dseq(inf, [0.2, 0.3, 0.4, Dseq(inf, [1, 1, 1, 2, 1, 2])]) / 2,
	Dust(1).kr,
	Dseq(inf, [0, 1, 2, Dseq(inf, [1, 2, 3, 4, 5])])
) * 30 + 250;
SinOsc(freq * [1, 1.01], 0) * 0.1
```

Demand rate reset:

```
var freq = DmdFor(
	Dseq(inf, [0.2, 0.3, 0.4, Dseq(inf, [1, 1, 1, 2, 1, 2])]) / 2,
	Dseq(inf, [1, 2, 4, 5]),
	Dseq(inf, [0, 1, 2, Dseq(inf, [1, 2, 3, 4, 5])])
) * 30 + 250;
SinOsc(freq * [1, 1.01], 0) * 0.1
```

Demand Ugen as audio oscillator:

```
var n = 5;
var m = 64;
var a = {
	var x = { 0.2.Rand2 } ! m;
	x := x ++ ({ Drand(1, { 0.2.Rand2 } ! n) } ! m.atRandom);
	Dseq(inf, x.scramble)
} ! n;
DmdFor(
	MouseX(1, 125, 1, 0.2) * SampleDur() * [1, 1.02],
	0,
	Dswitch1(a, MouseY(0, n - 1, 0, 0.2))
)
```
