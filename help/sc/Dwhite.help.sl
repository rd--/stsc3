# Dwhite -- noise generator

- _Dwhite(length, lo, hi)_

Demand rate white noise random generator.
Dwhite returns numbers in the continuous range between lo and hi.
The arguments can be a number or any other UGen.

- length: Number of values to create
- lo: Minimum value
- hi: Maximum value

Mouse controls range of frequency and rate of change.

```
var seq = Dwhite(inf, 0, MouseY(7, 45, 0, 0.2));
var trig = Impulse(MouseX(1, 40, 1, 0.2), 0);
var freq = Demand(trig, 0, seq) * 30 + 340;
SinOsc(freq, 0) * 0.1
```
