;; https://twitter.com/redFrik/status/839296011982024704
var t = LfSaw([0.5, 0.6], 0);
var f = LfSaw(5, 0) * LfSaw(0.015, 0) + 1 * 98;
var m = (2 ** LfSaw(4, 0)).roundTo(0.5) * 99;
var i = 2 ** LfSaw(1 / [8, 9], 0) * 8;
var g = GrainFm(1, t, 16, f, m, i, 0, -1, 512);
(g / 2).tanh * 0.25