;; deep trip (jmcc) #9
{
	var f = (LfNoise1(0.3.Rand) * 60 + 70).MidiCps;
	var a = LfNoise2(f * 0.5.Rand) * (LfNoise1(8.Rand) * SinOsc(40.Rand, 0) * 0.1).max(0);
	var s = Pan2(SinOsc(f, 0) * a, LfNoise1(5.Rand), 1);
	var c = { CombN(s, 0.5, { Rand(0.2, 0.5) } ! 2, 20) } ! 2;
	s + c.sum
}.overlap(12, 4, 4)
