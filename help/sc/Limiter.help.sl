# Limiter -- peak limiter

_Limiter(input, level, lookAheadTime)_

Limits the input amplitude to the given level. Limiter will not overshoot like _Compander_ will, but it needs to look ahead in the audio. Thus there is a delay equal to twice the lookAheadTime.

Limiter, unlike Compander, is completely transparent for an in range signal.

- input: the signal to be processed.
- level: the peak output amplitude level to which to normalize the input.
- lookAheadTime: the buffer delay time. Shorter times will produce smaller delays and quicker transient response times, but may introduce amplitude modulation artifacts.

Limit signal to -8db:

```
var t = Impulse(8, 0) * (LfSaw(0.25, 0) * -0.6 + 0.7);
var z = Decay2(t, 0.001, 0.3) * FSinOsc(500, 0);
var l = -8.DbAmp;
[z * l, Limiter(z, l, 0.01)]
```

Mouse control:

```
var t = Impulse(8, 0) * (LfSaw(0.25, 0) * -0.6 + 0.7);
var z = Decay2(t, 0.001, 0.3) * FSinOsc(500, 0);
Limiter(z, MouseX(0, 1, 0, 0.2), 0.01) * 0.2
```
