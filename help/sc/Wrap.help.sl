# Wrap -- wrap a signal outside given thresholds

_Wrap(in, lo, hi)_

This differs from _Wrap2_ in that it allows one to set both low and high thresholds.

- in: signal to be wrapped
- lo: low threshold of wrapping
- hi: high threshold of wrapping

Wrap a (-0.2, 0.2) sine tone to (-0.15, 0.15):

```
Wrap(SinOsc(440, 0) * 0.2, -0.15, 0.15)
```

Wrap frequency input:

```
var z = SinOsc(1/11, 0).Range(100, 200);
SinOsc(Wrap(z, [110, 130], [190, 170]), 0) * 0.1
```
