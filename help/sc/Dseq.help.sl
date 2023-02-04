# Dseq -- sequence generator

_Dseq(repeats, list)_

- repeats: number of repeats
- list: an array of values or other Ugens

Demand rate sequence generator.

Repeat sequence three times, mouse control of request frequency:

```
var seq = Dseq(3, [1, 3, 2, 7, 8]);
var trig = Impulse(MouseX(1, 40, 1, 0.2), 0);
var freq = Demand(trig, 0, seq) * 30 + 340;
SinOsc(freq, 0) * 0.1
```

Infinite repetitions of sequence of thirty-two random numbers, mouse control of request frequency with range into audio rate:

```
var seq = Dseq(inf, { 10.IRand } ! 32);
var trig = Impulse(MouseX(1, 10000, 1, 0.2), 0);
var freq = Demand(trig, 0, seq) * 30 + 340;
SinOsc(freq, 0) * 0.1
```
