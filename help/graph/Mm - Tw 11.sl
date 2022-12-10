;; sc-140 ; #11 ; Micromoog
var f0 = [[1, 2, 3, 4], [1, 2, 3, 4]];
var f1 = Hasher(Latch(SinOsc(f0, 0),Impulse([5 / 2, 5], 0))) * 300 + 300;
var z = VarSaw(f1.RoundTo(60), 0, 0.5) * (LfNoise2(2) * (1 / 3) + 0.5) / 5;
z.transpose.sum
