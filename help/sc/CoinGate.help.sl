# CoinGate -- statistical gate

_CoinGate(prob, trig)_

When it receives a trigger, it tosses a coin, and either passes the trigger or does not.

- prob: value between 0 and 1 determines probability of either possibilities
- trig: input signal

Mouse controls probablity:

```
var prob = MouseX(0, 1, 0, 0.2);
var trig = CoinGate(prob, Impulse(10, 0));
var freq = TRand([200, 300], [400, 900], trig);
SinOsc(freq, 0) * 0.1
```

Trigger level is preserved, mouse controls probablity over partial range:

```
var prob = MouseX(0, 0.65, 0, 0.2);
var trig = Impulse(20, 0) * (SinOsc(0.5, 0) + 1);
{
	Ringz(
		CoinGate(prob, trig * 0.5),
		[1, 1.5] * Rand(1000, 9000),
		0.01
	)
} !+ 3
```
