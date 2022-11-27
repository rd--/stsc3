;; GrayNoise
OverlapTexture({ :tr |
	var amp = LFPulse(4, 0, 0.1) * 0.002;
	var exc = LPZ1({ GrayNoise() } ! 2 * amp);
	Ringz(exc, { TRand(80, 400, tr) } ! 4, 1)
}, 4, 4, 2)
