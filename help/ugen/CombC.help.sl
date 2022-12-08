;; CombC ; noise burst as input signal ; function to create comb delays with random delay times
var n = 8;
var z = Decay2(
	in: Impulse(0.5, 0),
	attackTime: 0.01,
	decayTime: 0.20
) * PinkNoise() * 0.1;
{
	CombC(
		in: z,
		maxdelaytime: 0.1,
		delaytime: Rand(0.01, 0.09),
		decaytime: 3
	)
} !+ n
