# Sweep -- triggered linear ramp

_Sweep(trig, rate)_

Starts a linear raise by rate/sec from zero when trig input crosses from non-positive to positive

Using sweep to modulate sine frequency:

	var trig = Impulse(MouseX(0.5, 20, 1, 0.2), 0);
	SinOsc(Sweep(trig, 700) + 500, 0) * 0.1

Using sweep to index into a buffer:

	var trig = Impulse(MouseX(0.5, 10, 1, 0.2), 0);
	var sf = SfAcquire('floating_1', 1, [1]).first;
	BufRd(1, sf, Sweep(trig, BufSampleRate(sf)), 1, 2)

Backwards, variable offset:

	var trig = Impulse(MouseX(0.5, 10, 1, 0.2), 0);
	var sf = SfAcquire('floating_1', 1, [1]).first;
	var rate = BufSampleRate(sf);
	var ph0 = BufFrames(sf) * LfNoise0(0.2);
	var ph = Sweep(trig, rate.Neg) + ph0;
	BufRd(1, sf, ph, 1, 2)

Raising rate:

	var trig = Impulse(MouseX(0.5, 10, 1, 0.2), 0);
	var sf = SfAcquire('floating_1', 1, [1]).first;
	var rate = Sweep(trig, 2) + 0.5;
	BufRd(1, sf, Sweep(trig, BufSampleRate(sf) * rate), 1, 2)

