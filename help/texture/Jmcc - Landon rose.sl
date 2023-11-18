(* landon rose (jmcc) #8 ; simpler *)
var ringTimes = 3 ! 4;
var noteList = [
	32 43 54 89;
	10 34 80 120;
	67 88 90 100;
	14 23 34 45;
	76 88 99 124
];
{
	RingzBank(
		Release({ PinkNoise() } ! 2 * 0.001, 2, 1, 2),
		noteList.atRandom.MidiCps,
		1,
		ringTimes
	)
}.playEvery(2)
