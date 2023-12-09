(* cpiano (jmcc) ; graph rewrite ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
{ :tr |
	var dgr = DegreeToKey(
		[0 2 4 5 7 9 11].asLocalBuf,
		TRand(0, 12, tr),
		12
	);
	var freq = (60 + dgr).MidiCps;
	var pan = TRand(-0.25, 0.25, tr);
	var amp = TRand(0.1, 0.2, tr);
	var hammerEnv = Decay2(tr, 0.008, 0.04) * amp;
	var src = [0.997, 1.0, 1.002].collect { :detune |
		var hammer = LfNoise2(3000) * hammerEnv;
		var delayTime = (freq * detune).reciprocal;
		var decayTime = TRand(3, 6, tr);
		CombL(hammer, delayTime, delayTime, decayTime)
	}.Sum;
	EqPan2(src, pan) / 2
}.OverlapTexture(2, 0, 9).Mix
