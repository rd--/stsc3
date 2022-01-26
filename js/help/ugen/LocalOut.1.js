// localOut ; ping pong ; buffer coloring error: tried to release output with zero count ; output: 12 DelayC 0 ; input: BinaryOpUGen 0
var n, l, d, o;
n = mul(mul(Decay(Impulse(0.3, 0), 0.1), PinkNoise()), 0.2);
l = add(LocalIn(2, 0), [n, 0]);
d = DelayC(l, 0.2, 0.2);
o = LocalOut(mul(reverse(d), 0.8));
mrg(d, o)
