(* reso-pulse ; jmcc #1 ; graph rewrite *)
var lfoFreq = 6;
var lfo = LfNoise0(lfoFreq) * 1000 + 1200;
var left = Rlpf(
	{ :tr |
		var l = [25 30 34 37 41 42 46 49 53 54 58 61 63 66];
		var f = TChoose(tr, l).MidiCps;
		LfPulse(f, 0, 0.2) + LfPulse(2 * f + TRand(-0.5, 0.5, tr), 0, 0.2)
	}.OverlapTexture(4, 2, 4).Sum * 0.02,
	lfo, (* cutoff freq *)
	MouseX(0.2, 0.02, 1, 0.2) (* filter bandwidth *)
);
var delayTime = 2 / lfoFreq; (* delay right channel by two beats *)
var right = DelayC(left, delayTime, delayTime);
[left, right]
