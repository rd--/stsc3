;; sc-140 ; 15 ; Jason Dixon
var x = { MulAdd(Clip(LFNoise2(3), 0, 1), 0.02990, 0.00001).roundTo(0) } ! 5;
Splay2(Friction(LFTri(50, 0), x, 0.414, 0.313, x * 30000, 1))
