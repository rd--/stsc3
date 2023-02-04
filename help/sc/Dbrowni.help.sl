# Dibrown -- noise generator

- _Dibrown(length, lo, hi, step)_

- length: number of values to create
- lo: minimum value
- hi: maximum value
- step: maximum step for each new value

Demand rate brownian movement generator.
_Dibrown_ returns numbers in the continuous range between lo and hi.
The arguments can be a number or any other ugen.

```
var a = Dibrown(inf, 0, 15, 1);
var trig = Impulse(MouseX(1, 40, 1, 0.2), 0);
var freq = Demand(trig, 0, a) * 30 + 340;
SinOsc(freq, 0) * 0.1
```

* * *

See also: _Demand_, _Dbrown_, _Dwhite_
