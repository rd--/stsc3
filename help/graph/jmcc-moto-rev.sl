;; moto rev ; jmcc
Rlpf(LfPulse(MulAdd(SinOsc(0.2, 0), 10, 21), [0, 0.1], 0.1), 100, 0.1).Clip2(0.4)

;; moto rev (jmcc) #1
var f = SinOsc(0.2, 0) * 10 + 21;
var s = LfPulse(f, [0, 0.1], 0.1);
Rlpf(s, 100, 0.1).Clip2(0.4)
