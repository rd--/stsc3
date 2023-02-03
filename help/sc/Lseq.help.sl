# Lseq -- demand rate sequence generator

_Lseq(repeats, list)_

- repeats: number of repeats
- list: an array of values or other Ugens

Repeat sequence three times, mouse control of request frequency:

```
var a = Lseq(3, [1, 3, 2, 7, 8]);
var trig = Impulse(MouseX(1, 40, 1, 0.2), 0);
var freq = DmdOn(trig, 0, a) * 30 + 340;
SinOsc(freq, 0) * 0.1
```

Infinite repetitions of sequence of thirty-two random numbers, mouse control of request frequency with range into audio rate:

```
var a = Lseq(inf, { 10.IRand } ! 32);
var trig = Impulse(MouseX(1, 10000, 1, 0.2), 0);
var freq = DmdOn(trig, 0, a) * 30 + 340;
SinOsc(freq, 0) * 0.1
```
