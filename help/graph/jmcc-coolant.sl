;; coolant (jmcc)
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { Rand(40, 2040) } ! 10, [0.1], [1]) } ! 2

;; coolant (jmcc) ; default values
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { Rand(40, 2040) } ! 10, nil, nil) } ! 2 * 0.1

;; coolant (jmcc) ; Rand->rand
{ RingzBank(OnePole(BrownNoise() * 0.002, 0.95), { 40 + 2000.0.rand } ! 10, [0.1], [1]) } ! 2

;; coolant (jmcc) ; Ringz
var o = OnePole(BrownNoise() * 0.002, 0.95);
var f = { Ringz(o, Rand(40, 2040), 1) * 0.1 };
Splay2(f ! 10)

;; coolant (jmcc) ; filtermethods
{ BrownNoise().Mul(0.002).OnePole(0.95).RingzBank({ 40 + 2000.0.rand } ! 10, [0.1], [1]) } ! 2
