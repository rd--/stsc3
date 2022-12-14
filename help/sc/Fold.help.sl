# Fold -- fold a signal outside given thresholds

_Fold(in, lo, hi)_

This differs from _Fold2_ in that it allows one to set both low and high thresholds.

- in: signal to be folded
- lo: low threshold of folding, values < lo will be folded.
- hi: high threshold of folding, values > hi will be folded.

```
Fold(SinOsc(440, 0) * 0.2, -0.1, 0.1)
```

Modulate both fold points, at different ratios:

```
var x = MouseX(0.1, 0.2, 0, 0.2);
Fold(SinOsc(440, 0) * 0.2, x.Neg / 2, x)
```

Modulate one fold point:

```
var x = MouseX(0.1, 0.2, 0, 0.2);
Fold(SinOsc(440, 0) * 0.2, -0.2, x)
```
