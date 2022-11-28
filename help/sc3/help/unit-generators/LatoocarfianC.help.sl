# LatoocarfianC - latoocarfian chaotic generator

_LatoocarfianC(freq, a, b, c, d, xi, yi)_

- freq: iteration frequency in Hertz
- a, b, c, d: equation variables
- xi: initial value of x
- yi: initial value of y

A cubic-interpolating sound generator based on a function given in the Clifford Pickover book _Chaos In Wonderland_ on page 26.  The function is:

1. x(n+1) = sin(b * yn) + c * sin(b * xn)
2. y(n+1) = sin(a * yn) + d * sin(a * xn)

According to Pickover, parameters a and b should be in the range from -3 to +3, and parameters c and d should be in the range from 0.5 to 1.5.  The function can, depending on the parameters given, give continuous chaotic output, converge to a single value (silence) or oscillate in a cycle (tone).

This UGen is experimental and not optimized currently, so is rather hoggish of CPU.
