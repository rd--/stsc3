;; sc-140 ; 19 ; MCLD
var a = [0.02, 0.1, 1, 2, 3, 4];
var k = LfPar(a + 0.5, 0).sum;
var f = Latch(k, Impulse(a, 0));
Splay2(SinOsc(f * 100 + 300, 0) / 5)