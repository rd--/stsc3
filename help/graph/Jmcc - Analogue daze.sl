;; analogue daze (jmcc) #3
var pattern = [55, 63, 60, 63, 57, 65, 62, 65];
var f = { :octave :clockRate :pwmrate :fltrate |
	var sequ = { :s :tr | DmdOn(tr, 0, Lseq(inf, s)) };
	var tr = Impulse(clockRate, 0);
	var patternCps = (pattern + (12 * octave)).MidiCps;
	var sq = sequ(patternCps, tr);
	var pwm = SinOsc(pwmrate, Rand(0, 2 * pi)) * 0.4 + 0.5;
	var cf = SinOsc(fltrate, Rand(0, 2 * pi)) * 1400 + 2000;
	Rlpf(LfPulse(Lag(sq, 0.05), 0, pwm) * 0.1, cf, 1 / 15);
};
var a = { LfNoise0(LfNoise1(0.3) * 6000 + 8000) } ! 2 * 0.07;
var x = Decay(Impulse(2, 0), 0.15) * a;
var g = x + [f(1, 8, 0.31, 0.2), f(0, 2, 0.13, 0.11)];
CombC(g, 0.375, 0.375, 5) + g.reversed * 0.4
