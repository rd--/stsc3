// https://twitter.com/redFrik/status/839296011982024704
var t, f, m, i, g;
t = LFSaw([0.5, 0.6], 0);
f = mul(add(mul(LFSaw(5, 0), LFSaw(0.015, 0)), 1), 98);
m = mul(roundTo(pow(2, LFSaw(4, 0)), 0.5), 99);
i = mul(pow(2, LFSaw(fdiv(1, [8, 9]), 0)), 8);
g = GrainFM(1, t, 16, f, m, i, 0, -1, 512);
mul(tanh(fdiv(g, 2)), 0.25)
