# Pluck -- karplus-strong ugen

_Pluck(in, trig, maxdelaytime, delaytime, decaytime, coef)_

- in: an excitation signal
- trig: on a negative to positive transition fill delay line
- maxdelaytime: the max delay time in seconds (initializes the internal delay buffer).
- delaytime: delay time in seconds.
- decaytime: time for the echoes to decay by 60 decibels. Negative times emphasize odd partials.
- coef: the coef of the internal OnePole filter. Values should be between -1 and +1 (larger values will be unstable... so be careful!).

On _trig_, _n_ samples of the excitation signal are fed into the delay line, where _n = delaytime * SampleRate() / 2_.
The delay line is filled using a rectangular envelope, that is there is no fading.

Excitation signal is WhiteNoise, triggered twice a second with varying OnePole coef:

```
Pluck(
	WhiteNoise() * 0.1,
	Impulse(2, 0),
	1 / 440,
	1 / 440,
	10,
	MouseX(-0.999, 0.999, 0, 0.2)
)
```

Randomised duplicates:

```
var k = 50;
var freq = SinOsc(
	{ Rand(0.05, 0.2) } ! k,
	{ Rand(0, 1) } ! k
).Range(1000, 3000);
LeakDc(
	EqPan2(
		Pluck(
			WhiteNoise() * 0.1 ! k,
			Impulse({ Rand(10, 12) } ! k, 0),
			1 / 100,
			1 / freq,
			2,
			Rand(0.01, 0.2)
		),
		{ 1.Rand2 } ! k
	).sum,
	0.995
)
```
