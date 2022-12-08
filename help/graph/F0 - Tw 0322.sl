;; https://sccode.org/1-4Qy ; f0 ; 0322
var c = SinOscFb([1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144] / (SinOscFb(1 / [2, 4], 0) % 1 + 0.125).roundTo(1), 1);
var m = c > 0 * [9, 2, 3, 0, 7, 5] + 55;
Splay2(SinOscFb(m.MidiCps, SinOscFb(c / 999, 0) + 1 / 2)) / 3
