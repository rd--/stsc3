# Splay -- stereo panner

- _Splay(inArray, spread, level, center, levelComp)_

Splay spreads an array of channels across the stereo field.

Mouse control:

```
var freq = (0 .. 9).collect { :i |
	LfNoise2(Rand(10, 20)) * 200 + (i + 3 * 100)
};
Splay(
	SinOsc(freq, 0),
	MouseY(1, 0, 0, 0.2),
	0.5,
	MouseX(-1, 1, 0, 0.2),
	true
)
```
