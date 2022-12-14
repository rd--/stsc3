# RandRange -- random number generator

_RandRange(lo, hi)_

Generates random numbers between _lo_ and _hi_.

White noise:

```
var n = LfNoise2(1 / 3).Range(0.01, 0.1);
RandRange([Dc(-0.05), 0 - n], [Dc(0.05), n])
```
