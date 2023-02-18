# Gcd -- greatest common divisor

_Gcd(self, aNumber)_

This definition extends the usual definition and returns a negative number if both operands are negative.
This makes it consistent with the lattice-theoretical interpretation and its idempotency, commutative, associative, absorption laws.

"greater" means "divisible by" in this interpretation, so _Gcd(-1, -1)_ returns a negative number.
This is necessary to make the whole system consistent (fundamental law of arithmetics, idempotency and absorption laws would fail).

Following the example of the programming language _J_, Gcd is analogous to logical or.

```
var mx = MouseX(-200, 200, 0, 0.2);
var my = MouseY(-200, 200, 0, 0.2);
var freq = SinOsc(0.3, 0) * 20.Gcd([mx, my]) * 30 + 500;
SinOsc(freq, 0) * 0.1
```

