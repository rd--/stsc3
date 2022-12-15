;; string wander-cluster (jmcc) #6 ; with vibrato
{
	var note1 = 50 + 50.IRand;
	var note2 = Fold(note1 + 15.IRand - 7, 50, 120);
	var freq = note2.MidiCps;
	var decayTime = 1 / freq * 1000;
	var freq2 = SinOsc(4 + 4.Rand, 0) * (0.01 * freq) + freq;
	var delay = 1 / freq2;
	EqPan2(CombC(WhiteNoise() * 0.008, 0.01, delay, decayTime), 1.Rand2)
}.overlap(4 / 3, 4 / 3, 6)
