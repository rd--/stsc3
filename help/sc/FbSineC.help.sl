# FbSineC -- feedback sine with chaotic phase indexing

_FbSineC(freq, im, fb, a, c, xi, yi)_

- freq: iteration frequency in Hertz
- im: index multiplier amount
- fb: feedback amount
- a : phase multiplier amount
- c: phase increment amount
- xi: initial value of x
- yi: initial value of y

A cubic-interpolating sound generator based on the difference equations:

1. xn+1 = sin(im*yn + fb*xn)
2. yn+1 = (ayn + c) % 2pi

This uses a linear congruential function to drive the phase indexing of a sine wave. For im = 1, fb = 0, and a = 1 a normal sinewave results.
