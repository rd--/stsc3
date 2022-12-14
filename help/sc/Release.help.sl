# Release -- envelope generator

- _Release(in, attackTime, sustainTime, releaseTime)_

Apply a three stage envelope to _in_ and release the synthesis node once the envelope is ended.

```
var counter = PinkNoise() * Decay(Impulse(1, 0), 0.1) * 0.1;
Release(SinOsc(440, 0) * 0.1, 3, 3, 5) + counter
```

* * *

See also: _Asr_
