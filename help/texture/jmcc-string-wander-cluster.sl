;; string wander-cluster (jmcc) #6
{
	var note1 = 50 + 50.rand;
	var note2 = Fold(note1 + 15.rand - 7, 50, 120);
	var delay = 1 / note2.MidiCps;
	Pan2(CombC(WhiteNoise() * 0.008, 0.01, delay, delay * 1000), 1.0.rand2, 1)
}.overlap(4 / 3, 4 / 3, 6)

;; string wander-cluster (jmcc) #6 ; with vibrato
{
	var note1 = 50 + 50.rand;
	var note2 = Fold(note1 + 15.rand - 7, 50, 120);
	var freq = note2.MidiCps;
	var decayTime = 1 / freq * 1000;
	var freq2 = SinOsc(4 + 4.0.rand, 0) * (0.01 * freq) + freq;
	var delay = 1 / freq2;
	Pan2(CombC(WhiteNoise() * 0.008, 0.01, delay, decayTime), 1.0.rand2, 1)
}.overlap(4 / 3, 4 / 3, 6)
