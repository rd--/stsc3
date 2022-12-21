;; jmcc ; ostinoodles ; requires=TScramble
var z = OverlapTexture({ :tr |
	var Sequencer = { :s :tr |
		DmdOn(tr, 0, Seq(inf, s))
	};
	var root = 81 + TRand(-6, 6, tr);
	var major = [0, 2, 4, 5, 7, 9, 11].asLocalBuf;
	var offset = TRand(-16, 16, tr);
	var seq = DegreeToKey(
		major,
		TScramble(tr, [0, 1, 2, 3] + offset),
		12
	) + root;
	var f = TxLine(TExpRand(4, 24, tr), TExpRand(4, 24, tr), 12, tr);
	var trig = Impulse(f, 0);
	var freq = Sequencer(seq.MidiCps, trig);
	var sig = LfTri(freq.kr, 0) * Decay2(trig, 0.004, 0.3).kr * 0.1;
	EqPan2(sig, TRand(-1, 1, tr))
}, 6, 3, 6);
6.timesRepeat {
	z := AllpassN(z, 0.04, { Rand(0, 0.04) } ! 2, 16)
};
z
