;; tw 0033 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537
var f = LfPar(1 / 14, 0).RoundTo(1) * 20 + 80;
var a = Pulse([1, 2, 3, 4], 0.35);
var n = BrownNoise() * a;
var z = { :i | [i + 1 * f, i * f + i + 0.333] };
var o = LfPar((1 .. 4).collect(z), 0);
(o > n / 3).Splay2.sum * 0.1
