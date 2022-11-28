;; Rhpf ; requires=ArrayedEnv
var tr = Impulse([1/3, 1/5, 1/7, 1/9], 0);
Rhpf(PinkNoise() * Decay2(tr, 3, MouseX(3, 27, 0, 0.2)), TLine(300, [3, 9, 12, 15] * 1000, 3, tr), MouseY(0.1, 1, 0, 0.2)).Splay2 * 0.2
