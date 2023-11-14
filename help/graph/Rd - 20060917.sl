(* 20060917 ; rd ; requires=DustRange *)
var b = [
	60 71 89 65 36 57 92 97 92 97;
	71 89 60 57 65 36 95 92 93 97
].asLocalBuf;
var clk = DustRange(0.2, 0.9);
var env = Decay2(clk, 0.2, 2.5);
var idx = Stepper(clk, 0, 0, 15, 1, 0);
var f1 = (BufRd(2, b, idx, 1, 1) - 24).MidiCps;
var f2 = LfNoise0([1, 3]) * 1.2 + f1;
SinOsc(f1, 0) + SinOsc(f2, 0) * env * 0.2
