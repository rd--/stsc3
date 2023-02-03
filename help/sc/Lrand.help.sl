# Lrand -- demand rate random sequence generator

Lrand(repeats, list)

- repeats: number of values to return
- list: an array of values or other Ugens

```
{
	var seq = Lrand(inf, [1, 2, 3, 7, 8]);
	var trig = Impulse(MouseX(1, 60, 1, 0.2), 0);
	var freq = DmdOn(trig, 0, seq) * 30 + 340;
	SinOsc(freq, 0) * 0.1
} ! 2
```
