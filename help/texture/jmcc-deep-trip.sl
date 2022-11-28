;; deep trip (jmcc) #9
{
	var f = (LfNoise1(0.3.rand) * 60 + 70).MidiCps;
	var a = LfNoise2(f * 0.5.rand) * (LfNoise1(8.0.rand) * SinOsc(40.0.rand, 0) * 0.1).max(0);
	var s = Pan2(SinOsc(f, 0) * a, LfNoise1(5.0.rand), 1);
	var c1 = CombN(s, 0.5, { 0.2.rand + 0.3 } ! 2, 20);
	var c2 = CombN(s, 0.5, { 0.2.rand + 0.3 } ! 2, 20);
	s + c1 + c2
}.overlap(12, 4, 4)
