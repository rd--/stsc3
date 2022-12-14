# Tanh -- hyperbolic tangent

Tanh is a form of distortion:

```
SinOsc([440, 441], 0).Tanh * SinOsc([0.1, 0.25], 0) * 0.1
```

Increase amplitude over time:

```
var e = XLn(0.1, 10, 10);
var o = SinOsc(500, 0);
(o * e).Tanh * 0.25
```
