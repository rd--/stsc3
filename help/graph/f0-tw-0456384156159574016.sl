;; https://twitter.com/redFrik/status/456384156159574016
var a = 1 / [3, 12, 4, 1, 6, 2];
var s = Lag3(SinOsc(a, 0), SinOsc(2.67 ** a, 0).abs) * 99;
var f = ((SinOsc((1 / a) / 9, a) > 0) * 20 + 99) / a;
SinOsc(Hpf(Ringz(s, f, 1), 440), 0).Splay2 * 0.25
