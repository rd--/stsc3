# Wrap2 -- bilateral wrapping

_Wrap2(self, aNumber)_

Wraps input signal to +/- _aNumber_.

```
SinOsc(1000, 0).Wrap2(Ln(0, 1.01, 8)) * 0.1
```

* * *

See also: _Wrap_

SinOsc(440, 0) * ExpRandRange(0.01, LfNoise2(1 / 3).Range(0.01, 0.1))
