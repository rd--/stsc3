(* berlin 1977 (jmcc) #4 *)
|(
	sequ = { :s :tr | Demand(tr, 0, Dseq(inf, s)) },
	sequR = { :s :tr | Demand(tr, 0, Dshuf(inf, s)) },
	clockRate = MouseX(5, 20, 1, 0.2),
	clockTime = 1 / clockRate,
	clock = Impulse(clockRate, 0),
	patternList = [55 60 63 62 60 67 63 58],
	note = sequ(patternList, clock),
	clock16 = PulseDivider(clock, 16, 0),
	noteTrs = sequR([-12 -7 -5 0 2 5], clock16) + note,
	freq = noteTrs.MidiCps,
	env = Decay2(clock, 0.05 * clockTime, 2 * clockTime),
	amp = env * 0.1 + 0.02,
	filt = env * FSinOsc(0.17, 0) * 800 + 1400,
	pw = SinOsc(0.08, [0 0.5.pi]) * 0.45 + 0.5,
	s = Pulse(freq, pw) * amp
)|
CombC(Rlpf(s, filt, 0.15), 0.2, [0.2 0.17], 1.5)
