# ExpRand -- random number generator

_ExpRand(lo, hi)_

Generates a single random float value in an exponential distributions from _lo_ to _hi_.

```
var n = 5;
var freq = { ExpRand(110, 220) } ! n;
var ampl = { ExpRand(0.05, 0.10) } ! n;
var o = SinOsc(freq, 0) * ampl;
Splay2(o)
```
* * *

See also: _TExpRand_, _Rand_
