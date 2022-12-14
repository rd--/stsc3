# Schmidt -- Schmidt trigger

_Schmidt(in, lo, hi)_

When in crosses to greater than hi, output 1, then when signal crosses lower than lo output 0. Uses the formula if(out == 1, { if(in < lo, { out = 0.0 }) }, { if(in > hi, { out = 1.0 }) }). Output is initially zero.

- in: signal to be tested
- lo: low threshold
- hi: high threshold

Threshold octave jumps:

	var in = LfNoise1(3);
	var octave = Schmidt(in, -0.15, 0.15) + 1;
	SinOsc(in * 200 + 500 * octave, 0) * 0.1

