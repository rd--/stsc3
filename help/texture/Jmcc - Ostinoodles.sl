(* ostinoodles (jmcc) #9 *)
var root = 81 + (-6 .. 6).atRandom;
var major = [0 2 4 5 7 9 11];
{
	var offset = (-16 .. 16).atRandom;
	var seq = ([0 .. 3] + offset).shuffled.degreeToKey(major, 12) + root;
	var trig = Impulse(XLine(ExpRand(4, 24), ExpRand(4, 24), 12), 0);
	var f = Sequencer(seq.MidiCps, trig);
	var z = EqPan(LfTri(f, 0) * Decay2(trig, 0.004, 0.3) * 0.1, 1.Rand2);
	6.timesRepeat {
		z := AllpassN(z, 0.040, { 0.04.Rand } ! 2, 16)
	};
	z
}.overlap(6, 3, 6)
