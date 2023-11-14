(* cpiano (jmcc) ; graph rewrite ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
var nc = 2;
{ :tr |
	var dgr = DegreeToKey(
		[0 2 4 5 7 9 11].asLocalBuf,
		Rand(tr, 0, 12),
		12
	);
	var freq = (60 + dgr).MidiCps;
	var pan = Rand(tr, -0.25, 0.25);
	var amp = Rand(tr, 0.1, 0.2);
	var hammerEnv = Decay2(tr, 0.008, 0.04) * amp;
	var src = [0.997, 1.0, 1.002].collect { :detune |
		var hammer = LfNoise2(3000) * hammerEnv;
		var delayTime = (freq * detune).reciprocal;
		var decayTime = Rand(tr, 3, 6);
		CombL(hammer, delayTime, delayTime, decayTime)
	}.sum;
	PanAz(nc, src, pan, 1, 2, 0.5)
}.OverlapTexture(2, 0, 9).Mix
