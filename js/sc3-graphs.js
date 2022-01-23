var graphs = [
    mul(SinOsc(440, 0), 0.1),
    Pan2(SinOsc(440, 0), 0, 0.1),
    mul(SinOsc([440, 441], 0), 0.1),
    sum(Pan2(HPF(PinkNoise(), [3000, 11000]), SinOsc([1 / 7, 1 / 13], [0, pi]), 0.1)),
    mul(CombN(mul(SinOsc(midiCps(MulAdd(LFSaw(0.4, 0), 24, MulAdd(LFSaw([8, 7.23], 0), 3, 80))), 0), 0.04), 0.2, 0.2, 4), 0.1),
    mul(tanh(SinOsc([440, 441], 0)), mul(SinOsc([0.1, 0.25], 0), 0.1))
];
