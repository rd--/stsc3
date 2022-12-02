;; ostinoodles (jmcc) #9
var sequ = { :s :tr |	DmdOn(tr, 0, Seq(inf, s)) };
var root = 81 + (-6 .. 6).atRandom;
var major = [0, 2, 4, 5, 7, 9, 11];
{
	var offset = (-16 .. 16).atRandom;
	var sequence = (([0, 1, 2, 3] + offset).shuffled.degreeToKey(major, 12) + root).midiCps;
	var trig = Impulse(XLn(ExpRand(4, 24), ExpRand(4, 24), 12), 0);
	var f = sequ.value(sequence, trig);
	var z = Pan2(LfTri(f, 0) * Decay2(trig, 0.004, 0.3) * 0.1, Rand(-1, 1), 1);
	6.timesRepeat { z := AllpassN(z, 0.040, { Rand(0, 0.04) } ! 2, 16) };
	z
}.overlap(6, 3, 6)