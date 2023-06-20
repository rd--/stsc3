# Texture Programs

Texture programs generate sound by scheduling overlapping instances of a synthesiser program.

This program generates random panning three note chords.

```
var dur = 12;
{
	{
		Pan2(
			SinOsc(IRand(48, 72).MidiCps, 0),
			Ln(Rand(-1, 1), Rand(-1, 1), dur),
			Rand(0.01, 0.05)
		)
	} !+ 3
}.overlap(dur // 3, dur // 3, 4)
```

To stop a texture program the clock on which it is scheduled must be cleared.
The keybinding _Ctrl-Shift->_ clears the _workspace::clock_.

See also: _collectTexture_, _playEvery_ and _xfade_.
