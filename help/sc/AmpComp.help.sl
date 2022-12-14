# AmpComp -- psychoacoustic amplitude compensation

_AmpComp(freq, root, exp)_

Implements the (optimized) formula: _compensationFactor = (root / freq) ** exp_.

Higher frequencies are normally perceived as louder, which AmpComp compensates.

- freq: input frequency value. When _freq = root_, the output is 1
- root: root frequency relative to which the curve is calculated (usually lowest frequency)
- exp: exponent, how steep the curve decreases for increasing freq

Note that for frequencies very much smaller than root the amplitudes can become very high.
In this case limit the freq with _freq.Max(minval)_, or use _AmpCompA_.

Compare a sine tone without compensation with one that uses amplitude compensation:

```
var freq = MouseX(300, 15000, 1, 0.2);
var osc = SinOsc(freq, 0) * 0.1;
[osc, osc * AmpComp(freq, 300, 1/3).kr]
```

Different sounds cause quite different loudness perception,
and the desired musical behavior can vary,
so the exponent can be tuned:

```
var freq = MouseX(300, 15000, 1, 0.2);
Pulse(freq, 0.5) * 0.1 * AmpComp(freq, 300, 4 / 3).kr
```
