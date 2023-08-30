(* reso-pulse ; jmcc #1 ; move post-processing inwards ; requires timed scheduling at synthesiser *)
{
	var lfoFreq = 6;
	var lfo = LfNoise0(lfoFreq) * 1000 + 1200;
	var f = [25, 30, 34, 37, 41, 42, 46, 49, 53, 54, 58, 61, 63, 66].atRandom.MidiCps;
	var z = LfPulse(f, 0, 0.2) + LfPulse(2 * f + Rand(-0.5, 0.5), 0, 0.2);
	var left = Rlpf(
		z * 0.02,
		lfo, (* cutoff freq *)
		MouseX(0.2, 0.02, 1, 0.2) (* filter bandwidth *)
	);
	var delayTime = 2 / lfoFreq;
	var right = DelayC(left, delayTime, delayTime); (* delay right channel by two beats *)
	[left, right]
}.overlap(4, 2, 4)
