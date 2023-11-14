(* jmcc ; ostinoodles ; requires=TScramble ; requires=kr *)
var z = { :tr |
	var Sequencer = { :s :tr |
		Demand(tr, 0, Dseq(inf, s))
	};
	var root = 81 + Rand(tr, -6, 6);
	var major = [0 2 4 5 7 9 11].asLocalBuf;
	var offset = Rand(tr, -16, 16);
	var seq = DegreeToKey(
		major,
		Scramble(tr, [0 .. 3] + offset),
		12
	) + root;
	var f = XLine(tr, ExpRand(tr, 4, 24), ExpRand(tr, 4, 24), 12);
	var trig = Impulse(f, 0);
	var freq = Sequencer(seq.MidiCps, trig);
	var sig = LfTri(freq.kr, 0) * Decay2(trig, 0.004, 0.3).kr * 0.1;
	EqPan2(sig, Rand(tr, -1, 1))
}.OverlapTexture(6, 3, 6).Mix;
6.timesRepeat {
	z := AllpassN(z, 0.04, { Rand(0, 0.04) } ! 2, 16)
};
z
