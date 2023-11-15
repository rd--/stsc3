(* https://scsynth.org/t/6989/16 ; tm *)
{
	var num = 20;
	var dur = 1 / 60;
	var osc = {
		var freq = Rand(30, 5000) * (SinOsc(LfNoise1(dur / 6).Range(30, 1000), 0) * LfNoise1(dur).Range(0.01, 8)).MidiRatio;
		var sig = Select(LfNoise0(dur).Range(0, 2).rounded, [SinOsc(freq, 0), Saw(freq), Pulse(freq, 0)] * 0.1);
		var filtFreq = Rand(30, 1000) * LfNoise1(dur).Range(1, 5);
		var filt = Select(LfNoise0(dur).Range(0, 2).rounded, [Lpf(sig, filtFreq), Hpf(sig, filtFreq), Bpf(sig, filtFreq, 1)]);
		CombL(sig, 0.5, LfNoise1(dur / 6).Range(0.02, 0.5), Rand(0.3, 2))
	} ! num;
	var sig = osc.Splay2;
	FreeVerb2(sig.first, sig.second, LfNoise1(dur).Range(0, 1), LfNoise1(dur * 3).Range(0.2, 2), 0.5)
}.overlap(9, 3, 2)
