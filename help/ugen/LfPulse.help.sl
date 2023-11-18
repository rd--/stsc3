(* LfPulse ; phase value = (0, 1), offset to lowest point *)
LfPulse(110, 1 * 0.5, 0.5) * 0.1

(* LfPulse ; as envelope *)
SinOsc(200, 0) * Lag(LfPulse(7.83, 0, 0.5) > 0, 0.05) * 0.2

(* LfPulse ; as envelope *)
SinOsc(230, 0) * Lag(LfPulse(MouseX(2.3, 23, 1, 0.2), 0, 0.5).Max(0), 0.01) * 0.2

(* LfPulse ; 50 Hz wave *)
LfPulse(50, 0, 0.5) * 0.05

(* LfPulse ; modulating frequency *)
LfPulse(XLine(1, 200, 10), 0, 0.2) * 0.05

(* LfPulse ; amplitude modulation *)
LfPulse(XLine(1, 200, 10), 0, 0.2) * SinOsc(440, 0) * 0.1

(* LfPulse ; used as both oscillator and lfo *)
LfPulse(LfPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.05

(* LfPulse ; humm *)
var freqBass = 50;
var freq = 50;
var pan = 0;
var amp = 0.1;
var snd = LfPulse(freq, 0, 0.5);
20.timesRepeat {
	snd := MidEq(snd, ExpRand(300, 12000), 0.1, -20)
};
snd := Hpf(snd * 3, MouseX(5000, 7000, 1, 0.2));
snd := Lpf(snd, MouseY(9000, 11000, 1, 0.2));
snd := snd + SinOsc(freqBass, 0);
EqPan2(snd, pan) * amp

(* ---- LfPulse ; jmcc ; process (Eval) *)
{
	var p1 = LfPulse(ExpRand(0.1, 1), 0, 0.3) * 0.2 + 0.02;
	var in = LfSaw([21000, 21001], 0) * p1;
	var sr = ExpRand(300, 3000) + [-0.6, 0.6];
	var p2 = LfPulse(sr, 0, MouseY(0.01, 0.99, 0, 0.2));
	var p3 = LfPulse(ExpRand(0.1,12), 0, 0.7) * 0.2;
	var p4 = LfPulse(ExpRand(0.1,12), 0, 0.4) * 0.2 + 0.2 + p3;
	Rlpf(in * p2, sr * p4, 0.1)
}.overlap(4, 4, 4)
