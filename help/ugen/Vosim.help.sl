;; Vosim
Splay2(Vosim(Impulse(110 + [-1, 0, 1], 0), MouseX([110, 220, 440], 880, 1, 0.2), [3, 5, 7], [0.77, 0.88, 0.99])) * 0.1

;; Vosim
Splay2(Vosim(Impulse(110 + [-1, 0, 1], 0), LinExp(LfNoise2([0.35, 0.25, 0.15]), -1, 1, [110, 220, 440], 880), [3, 5, 7], [0.77, 0.88, 0.99])) * 0.1

;; Vosim
var p = TRand(0, 1, Impulse([3, 7], 0));
var t = Impulse([9, 27] * (1 + (p > 0.95)), 0);
var f = TRand([40, 120, 220], [440, 990, 880], t);
var n = TRand(2, [4, 8, 12], t);
var d = TRand([0.2, 0.4, 0.6], [0.6, 0.8, 1], t);
var a = TRand(0, [0.2, 0.6, 1], t);
var l = TRand(-1, 1, t);
var x = MouseX(0.25, 2, 0, 0.2);
var y = MouseY(0.25, 0.85, 0, 0.2);
var z = 9;
var freq = f * x * LinLin(LfNoise2(z), -1, 1, 0.25, 2);
var decay = d * y * LinLin(LfNoise2(z), -1, 1, 0.25, 2);
Pan2((Vosim(t, freq, n, decay) * a).sum, l, 0.25).sum
