;; https://twitter.com/lukiss163/status/1581607231463731200 ; lnu ; requires=SfAcquire
var b = SfAcquire("floating_1", 1, [1]);
var n = [-36, -9, -14, 0, -19, -5, 3, -2, -24, -7];
var k = n.size;
var r = { LFDNoise3(1 / 86).abs };
var w = Warp1(1, b, r ! k, n.midiRatio, r ! k * 8 + 8 / 86, -1, 12, 1 / 4 * r.dup(k), 4) * r.dup(k);
LeakDC(FreeVerb(w, r ! k, r ! k + 0.5, r.dup ! k), 0.995).Splay2.tanh
