;; sprinkler ; jmcc
Bpz2(WhiteNoise() * LfPulse(MulAdd(LfPulse(0.09, 0, 0.16), 10, 7), 0, 0.25) * 0.1)

;; sprinkler (jmcc) #1
Bpz2(WhiteNoise() * LfPulse(LfPulse(0.09, 0, 0.16) * 10 + 7, 0, 0.25) * 0.1)
