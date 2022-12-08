;; bowed string (jmcc) ; texture ; graph rewrite
OverlapTexture({ :tr |
	var root = 5;
	var scale = [0, 2, 4, 5, 7, 9, 11] + root;
	var oct = [24, 36, 48, 60, 72, 84];
	var f = (TChoose(tr, scale) + TChoose(tr, oct)).MidiCps;
	var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(TExpRand(0.125, 0.5, tr)) * 0.6 + 0.4).max(0);
	var k = RingzBank(x, Array.series(12, f, f), Array.geom(12, 1, TRand(0.7, 0.9, tr)), { TRand(1, 3, tr) } ! 12);
	(k * 0.1).SoftClip
}, 5, 2, 12)

;; bowed string (jmcc)
var root = 5;
var scale = [0, 2, 4, 5, 7, 9, 11] + root;
var oct = [24, 36, 48, 60, 72, 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).max(0);
var k = RingzBank(x, Array.series(12, f, f), Array.geom(12, 1, Rand(0.7, 0.9)), { Rand(1, 3) } ! 12);
(k * 0.1).SoftClip

;; bowed string (jmcc) ; .random
var root = 5;
var scale = [0, 2, 4, 5, 7, 9, 11] + root;
var oct = [24, 36, 48, 60, 72, 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).max(0);
var k = RingzBank(x, Array.series(12, f, f), Array.geom(12, 1, 0.7.randomFloat(0.9)), { 1.randomFloat(3) } ! 12);
(k * 0.1).SoftClip

;; bowed string (jmcc) ; klank
var root = 5;
var scale = [0, 2, 4, 5, 7, 9, 11] + root;
var oct = [24, 36, 48, 60, 72, 84];
var f = (scale.atRandom + oct.atRandom).MidiCps;
var x = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.6 + 0.4).max(0);
var d = [Array.series(12, f, f), Array.geom(12, 1, Rand(0.7, 0.9)), { Rand(1, 3) } ! 12].transpose.concatenation;
var k = Klank(x, 1, 0, 1, d);
(k * 0.1).SoftClip
