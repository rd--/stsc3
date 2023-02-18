# Hypot -- hypotenuse

_Hypot(self, aNumber)_

Answer the square root of the sum of the squares of the arguments. Or equivalently, the distance from the origin to the point _(x, y)_.

In this example, _Hypot_ is used to calculate a doppler shift pitch and amplitude based on distance.
Object travels 200 meters in 6 secs (=120kph) passing 10 meters from the listener.
(The speed of sound is 344 meters/sec.)

```
var x = 10;
var y = LfSaw(1 / 6, 0) * 100;
var distance = Hypot(x, y);
var velocity = Slope(distance);
var pitchRatio = (344 - velocity) / 344;
var amplitude = 10 / distance.Squared;
FSinOsc(1000 * pitchRatio, 0) * amplitude
```

The next example uses the distance to modulate a delay line:

```
var x = 10;
var y = LfSaw(1 / [6, 11], [0, 1]) * 100;
var distance = Hypot(x, y);
var amplitude = 40 / distance.Squared;
var motorSound = Rlpf(
	FSinOsc(200, 0) * LfPulse([31.3, 23.1], 0, 0.4),
	400,
	0.3
);
DelayL(motorSound, 110 / 344, distance / 344) * amplitude
```
