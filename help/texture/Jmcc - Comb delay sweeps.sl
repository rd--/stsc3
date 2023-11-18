(* comb delay sweeps (jmcc) #6 *)
{
	var dur = 4;
	var note1 = 50 + 70.Rand;
	var endNote1 = (note1 + 15.Rand - 7).Fold(50, 120);
	var note2 = (note1 + 15.Rand - 7).Fold(50, 120);
	var endNote2 = (endNote1 + 15.Rand - 7).Fold(50, 120);
	var noteSweep = Line(note2, endNote2, dur);
	var dt = 1 / noteSweep.MidiCps;
	var dc = 1 / note2.MidiCps * 1000;
	EqPan(CombC(WhiteNoise() * 0.005, 0.01, dt, dc), 1.Rand2)
}.overlap(4 / 3, 4 / 3, 6)
