;; scratchy ; jmcc
Rhpf(({ BrownNoise() } ! 2 * 0.5 - 0.49).max(0) * 20, 5000, 1)

;; scratchy (jmcc) #1
var n = { BrownNoise() } !2 * 0.5 - 0.49;
var f = n.max(0) * 20;
Rhpf(f, 5000, 1)

;; scratchy ; jmcc ; left-to-right
{ BrownNoise() }.dup(2).MulAdd(0.5, -0.49).Max(0).Mul(20).Rhpf(5000, 1)