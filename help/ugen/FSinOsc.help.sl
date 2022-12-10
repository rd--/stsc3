;; FSinOsc
var freq = [LfNoise0([3, 5]), FSinOsc([3, 5], 0)].atRandom * 500 + 800;
var amp = [LfNoise1(2.2).Max(0) * 0.5, FSinOsc(2.2, 0) * 0.25 + 0.25].atRandom;
[SinOsc(freq, 0), Blip(freq, 8)].atRandom * amp * 0.2
