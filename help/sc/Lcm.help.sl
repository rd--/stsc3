# Lcm -- least common multiple

This definition extends the usual definition and returns a negative number if any of the operands is negative.
This makes it consistent with the lattice-theoretical interpretation and its idempotency, commutative, associative, absorption laws.

Following the example of the programming language _J_, Lcm is analogous to logical and.

```
var mx = MouseX(-200, 200, 0, 0.2);
var my = MouseY(-200, 200, 0, 0.2);
var freq = SinOsc(0.3, 0) * 20.Lcm([mx, my]) * 30 + 500;
SinOsc(freq, 0) * 0.1
```

