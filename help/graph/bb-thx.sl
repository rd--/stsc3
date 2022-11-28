;; http://earslap.com/article/recreating-the-thx-deep-note.html ; 30 oscillators together, distributed across the stereo field
var numVoices = 30;
var fundamentals = { Rand(200, 400) } ! numVoices;
fundamentals.collect({ :freq |
	Pan2(
		Saw(freq),
		Rand(-0.5, 0.5),
		numVoices.reciprocal
	)
}).sum

;; http://earslap.com/article/recreating-the-thx-deep-note.html ; adding random wobbling to freqs, sorting randoms, lowpassing ; fundamentals are sorted, so higher frequencies drift more
var numVoices = 30;
var fundamentals = { 200.0.Rand(400) }.dup(numVoices).sorted;
fundamentals.withIndexCollect({ :freq0 :index |
	var freq = freq0 + (LfNoise2(0.5) * 3 * index);
	Pan2(
		BLowPass(Saw(freq), freq * 5, 0.5),
		Rand(-0.5, 0.5),
		numVoices.reciprocal
	)
}).sum

;; http://earslap.com/article/recreating-the-thx-deep-note.html ; inverting init sort, louder bass, final volume envelope, some little tweaks ; requires=CurveGen
var numVoices = 30;
var fundamentals = { 200.0.rrand(400.0) }.dup(numVoices).sorted.reversed;
var finalPitches = ((1 .. numVoices).collect { :each | (each / (numVoices / 6)).rounded * 12 } + 14.5 ).MidiCps;
var outerEnv = CurveGen(1, [0, 0.1, 1], [8, 4], [2, 4]);
var ampEnvelope = CurveGen(1, [0, 1, 1, 0], [3, 21, 3], [2, 0, -4]);
var voiceFunc = { :numTone |
	var initRandomFreq = fundamentals[numTone] + (LfNoise2(0.5) * 6 * (numVoices - numTone));
	var destinationFreq = finalPitches[numTone] + (LfNoise2(0.1) * numTone / 3);
	var sweepEnv = CurveGen(1, [0, Rand(0.1, 0.2), 1], [Rand(5.5, 6), Rand(8.5, 9)], [Rand(2, 3), Rand(4, 5)]);
	var freq = ((1 - sweepEnv) * initRandomFreq) + (sweepEnv * destinationFreq);
	Pan2(
		BLowPass(Saw(freq), freq * 6, 0.6),
		Rand(-0.5, 0.5),
		(1 - (1 / numTone)) * 1.5
	) / numVoices
};
var snd = (1 .. numVoices).collect(voiceFunc).sum;
Limiter(BLowPass(snd, 2000 + (outerEnv * 18000), 0.5) * (2 + outerEnv) * ampEnvelope, 1, 0.01)
