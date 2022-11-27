;; BufRd ; requires=kr
var numFrames = 2 * 48000;
var buf = BufAlloc(1, numFrames);
var osc = SinOsc(LFNoise1(2).kr * 300 + 400, 0) * 0.1;
var wr = BufWrite(buf, Phasor(0, 1, 0, numFrames, 0), 1, osc);
BufRd(1, buf, Phasor(0, 1, 0, numFrames, 0), 1, 2) <! wr

;; BufRd ; phasor as phase input ; mono sound file ; multiple voices
var buf = SfAcquire("crotale-d6", 1, 1);
{
	var tr = Impulse(2 ** Rand(1, 3) / BufDur(buf), 0).kr;
	var mnn = TIRand(-3, 0, tr) * 12 + TChoose(tr, [0, 2.1, 4.9, 7, 9.2]);
	var rt = mnn.MidiRatio * BufRateScale(buf);
	var ph = Phasor(tr, rt, 0, BufFrames(buf), 0);
	BufRd(1, buf, ph, 0, 2)
}.dup(6).Splay2 * 0.25
