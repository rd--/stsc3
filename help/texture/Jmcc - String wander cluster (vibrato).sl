(* string wander-cluster (jmcc) #6 ; with vibrato *)
{
	var note1 = IRand(50, 100);
	var note2 = Fold(note1 + 15.IRand - 7, 50, 120);
	var freq = note2.MidiCps;
	var decayTime = 1 / freq * 1000;
	var freq2 = SinOsc(Rand(4, 8), 0) * (0.01 * freq) + freq;
	var delay = 1 / freq2;
	EqPan(CombC(WhiteNoise() * 0.008, 0.01, delay, decayTime), Rand(-1, 1))
}.overlap(4 / 3, 4 / 3, 6)
