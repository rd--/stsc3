;; max
SinOsc(XLn([100, 200], [400, 100], 2).injectInto(0, { :i :j | i.max(j) }), 0) * 0.1

;; max
var freq = { LfNoise2(1) } ! 3 * 100 + 200;
var freqMax = freq.injectInto(0, { :i :j | i.max(j) });
(LfTri(freqMax, 0) * 0.05) + (SinOsc(freq, 0).Splay2 * 0.1)