;; https://twitter.com/lukiss163/status/1581607231463731200 ; lnu ; requires=SfAcquire
var b = SfAcquire("floating_1", 1, [1]);
var n = [-36, -9, -14, 0, -19, -5, 3, -2, -24, -7];
var k = n.size;
var r = { LfdNoise3(1 / 86).abs };
var w = r ! k * Warp1(1, b, r ! k, n.MidiRatio, r ! k * 8 + 8 / 86, -1, 12, r ! k / 4, 4);
LeakDc(FreeVerb(w, r ! k, r ! k + 0.5, r ! k), 0.995).Splay2.tanh
