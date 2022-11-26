;; berlin 1977 (jmcc) #4
var sequ = { :s :tr | DmdOn(tr, 0, Seq(inf, s)) };
var sequR = { :s :tr | DmdOn(tr, 0, Shuf(inf, s)) };
var clockRate = MouseX(5, 20, 1, 0.2);
var clockTime = 1 / clockRate;
var clock = Impulse(clockRate, 0);
var patternList = [55, 60, 63, 62, 60, 67, 63, 58];
var note = sequ(patternList, clock);
var clock16 = PulseDivider(clock, 16, 0);
var noteTrs = sequR([-12, -7, -5, 0, 2, 5], clock16) + note;
var freq = noteTrs.MidiCps;
var env = Decay2(clock, 0.05 * clockTime, 2 * clockTime);
var amp = env * 0.1 + 0.02;
var filt = env * FSinOsc(0.17, 0) * 800 + 1400;
var pw = SinOsc(0.08, [0, 0.5 * pi]) * 0.45 + 0.5;
var s = Pulse(freq, pw) * amp;
CombC(RLPF(s, filt, 0.15), 0.2, [0.2, 0.17], 1.5)

;; berlin 1977 (jmcc) #4 ; event control
Voicer(16, { :e |
	var freq = e.p.unitCps;
	var env = Decay2(Trig(e.w, 0.001), 0.05 * e.y, 2 * e.y);
	var amp = env * e.z + 0.02;
	var filt = env * (FSinOsc(0.17, 0) * 800) + 1400;
	var pw = SinOsc(0.08, [0, 0.5 * pi]) * 0.45 + 0.5;
	var s = Pulse(freq, pw) * amp;
	CombC(RLPF(s, filt, 0.15), 0.2, [0.2, 0.17], 1.5) * LagUD(e.w, 0, 2 + e.y)
}).sum

