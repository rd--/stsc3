;; https://sccode.org/1-4Qy ; f0 ; 0334
var b = [9 .. 1] * 4.0015;
var ph = SinOsc(2 / b, 0) * 3 + 4 ** SinOsc(SinOsc(b / 8, 0 ) / 2 + (SinOsc(1 / 33, 0 ) > 0 + 8 * b), 0) * 6;
Splay2(SinOsc(0, ph) * (4 ** SinOsc(1/b, b)).tanh / 2)
