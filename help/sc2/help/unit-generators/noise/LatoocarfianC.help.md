# LatoocarfianC - chaotic function

_LatoocarfianC(freq, a, b, c, d, xi, yi)_

This is a function given inClifford Pickover's book Chaos In Wonderland, pg 26.  The function has four parameters a, b, c, and d.  The function is _x1 = sin(b*y0) + c*sin(b*x0)\ny1 = sin(a*x0) + d*sin(a*y0)_.

According to Pickover, parameters a and b should be in the range from -3 to +3, and parameters c and d should be in the range from 0.5 to 1.5.  The function can, depending on the parameters given, give continuous chaotic output, converge to a single value (silence) or oscillate in a cycle (tone).  This UGen is experimental and not optimized currently, so is rather hoggish of CPU.

	// LatoocarfianC ; texture
	OverlapTexture({
		arg tr;
		var freq = TRand(400, SampleRate() / 3, tr);
		var a = TRand(-3, 3, tr);
		var b = TRand(-3, 3, tr);
		var c = TRand(0.5, 1.5, tr);
		var d = TRand(0.5, 1.5, tr);
		SinOsc(freq, 0) * 0.05 + Pan2(LatoocarfianC(freq, a, b, c, d, 0.5, 0.5), TRand(-1, 1, tr), 0.05)
	}, 1, 4, 8)

