(* https://schollz.com/blog/phasedistortion/ ; requires=voicer *)
Voicer(16) { :e |
	var freq = (e.x * 36 + 48).MidiCps; (* e.p.unitCps *)
	var amp = e.z;
	var freqBase = freq;
	var freqRes = LinLin(SinOsc(Rand(0, 0.2), 0), -1, 1, freqBase / 2, freqBase * 2);
	var pdBase = Impulse(freqBase, 0);
	var twoPi = 2 * pi;
	var pd = Phasor(pdBase, twoPi * freqBase / SampleRate(), 0, twoPi, 0);
	var pdRes = Phasor(pdBase, twoPi * freqRes / SampleRate(), 0, twoPi, 0);
	var pdi = LinLin((twoPi - pd).Max(0), 0, twoPi, 0, 1);
	var snd = Lag(SinOsc(0, pdRes) * pdi, 1 / freqBase);
	var env = Decay2(Trig(e.w, SampleDur()), 0.005, 10);
	EqPan2(snd, e.i * 2 - 1) * env * amp
}.Mix

