var graphs = [
    Mul(SinOsc(440, 0), 0.1),
    Pan2(SinOsc(440, 0), 0, 0.1),
    Mul(SinOsc([440, 441], 0), 0.1),
    Sum(Pan2(HPF(PinkNoise(), [3000, 11000]), SinOsc([1 / 7, 1 / 13], [0, pi]), 0.1)),
    Mul(CombN(Mul(SinOsc(MidiCps(MulAdd(LFSaw(0.4, 0), 24, MulAdd(LFSaw([8, 7.23], 0), 3, 80))), 0), 0.04), 0.2, 0.2, 4), 0.1),
    Mul(Tanh(SinOsc([440, 441], 0)), Mul(SinOsc([0.1, 0.25], 0), 0.1))
];
