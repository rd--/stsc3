;; ---- SfRead ; phasor as phase input ; requires=SfAcquire
var sf = SfAcquire('piano-c5', 2, [1, 2]);
var tr = Impulse(1 / SfDur(sf), 0);
var ph = Phasor(tr, SfRateScale(sf), 0, SfFrames(sf), 0);
SfRead(sf, ph, 0, 2)

;; SfRead ; phasor as phase input ; piano
var sf = SfAcquire('piano-c5', 2, [1, 2]);
{
	var tr = Impulse(2 ** Rand(1, 2) / SfDur(sf), 0).kr;
	var mnn = TiRand(-2, 0, tr) * 12 + TChoose(tr, [-3, 0, 2, 5, 7]);
	var rt = mnn.MidiRatio * SfRateScale(sf);
	var ph = Phasor(tr, rt, 0, SfFrames(sf), 0);
	SfRead(sf, ph, 0, 2)
} !+ 6 / 4

;; SfRead ; phasor as phase input ; harp
var sf = SfAcquire('harp-a4', 2, [1, 2]);
{
	var tr = Impulse(2 ** Rand(1, 3) / (SfDur(sf) * 4), 0).kr;
	var mnn = TiRand(-2, 0, tr) * 12 + TChoose(tr, [0, 2.1, 4.9, 7, 9.2]);
	var rt = mnn.MidiRatio * SfRateScale(sf);
	var ph = Phasor(tr, rt, 0, SfFrames(sf), 0);
	SfRead(sf, ph, 0, 2)
} !+ 6 / 9

;; SfRead ; phasor as phase input ; mono sound file ; multiple voices
var sf = SfAcquire('crotale-d6', 1, [1, 1]);
{
	var tr = Impulse(2 ** Rand(1, 3) / SfDur(sf), 0).kr;
	var mnn = TiRand(-3, 0, tr) * 12 + TChoose(tr, [0, 2.1, 4.9, 7, 9.2]);
	var rt = mnn.MidiRatio * SfRateScale(sf);
	var ph = Phasor(tr, rt, 0, SfFrames(sf), 0);
	SfRead(sf, ph, 0, 2)
} !+ 6 / 4
