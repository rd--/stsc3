(* vibraphone simulation ; Kevin Larke ; Real Time Vibraphone Pitch and Timbre Classification ; requires=voicer *)
Voicer(16) { :e |
	var freq = e.p.UnitCps;
	var detune = e.y * 15;
	var freqs = freq * [1, 4, 10, 13.75 + detune];
	var hiFreqs = freq * [19.2, 20, 21.2];
	var modFreq = e.j * 4 + 3;
	var modAmp = e.k * 2;
	var mod = LinLin(SinOsc(modFreq, Rand(e.w, 0, 2 * pi)), -1, 1, 1 - ([0.5, 0.3, 0.3, 0.5] * modAmp), 1);
	var velocity = Latch(e.z, e.w) * 2;
	var sound = 0.3 * [1, 1.1 * velocity, 0.6 * velocity * velocity, 0.5 * velocity] * SinOsc(freqs, 0);
	var decay = 0;
	var decays = [1, 0.7 * (-1 * decay).exp, 0.5 * (-1 * decay).exp, 0.4 * (-1 * decay).exp];
	var zero = 0.000001;
	var soundMain = XLine(e.w, zero, 1, 1 / freqs) * XLine(e.w, 1, zero, decays * 4) * sound * mod;
	var soundHigh = XLine(e.w, 0.3 * (velocity + zero), zero, 0.02) * SinOsc(hiFreqs, 0); (* hi frequency onset *)
	var pan = e.i * 2 - 1;
	EqPan2(soundMain.sum + soundHigh.sum, pan) * LagUd(e.w, 0.01, 4)
}.Mix * 0.25
