(* FbSineC ; default initial params *)
FbSineC(SampleRate() / 4, 1, 0.1, 1.1, 0.5, 0.1, 0.1) * 0.1

(* FbSineC ; increase feedback *)
FbSineC(SampleRate(), 1, Line(0.01, 4, 10), 1, 0.1, 0.1, 0.1) * 0.1

(* FbSineC ; increase phase multiplier *)
FbSineC(SampleRate(), 1, 0, XLine(1, 2, 10), 0.1, 0.1, 0.1) * 0.1

(* FbSineC ; modulate frequency and index multiplier *)
FbSineC(LfNoise2(1) * 1000 + 1000, LfNoise2(1) * 16 + 17, 1, 1.005, 0.7, 0.1, 0.1) * 0.1

(* FbSineC ; randomly modulate params *)
FbSineC(
	LfNoise2([1, 2]) * 1000 + 10000,
	LfNoise2([1, 2]) * 32 + 33,
	LfNoise2([1, 2]) * 0.5,
	LfNoise2([1, 2]) * 0.05 + 1.05,
	LfNoise2([1, 2]) * 0.3 + 0.3,
	0.1,
	0.1
) * 0.05
