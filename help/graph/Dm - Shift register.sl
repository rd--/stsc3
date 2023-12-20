(* ---- shift register (dm) ; https://sccode.org/1-590 ; erratic ? *)
var amp = 0.1;
var pulse = Impulse(1 / 16, 0);
var rate = Choose(pulse, [3, 5, 10]);
var trans = Choose(pulse, [0, 2, -2, 7, -5]);
var trig = Trig1(CuspL(rate.kr * 3, 1, 1.9, 0), 0.001);
var octave = Demand(PulseDivider(trig, 4, 0), 0, Drand(inf, [12, -12]));
var note = Demand(trig, 0, Dseq(inf, [42, 46, 51, 54, 59, 63, 66].shuffled + trans.kr) + octave);
var length = 5;
var buf = BufAlloc(1, length);
var count = PulseCount(trig, 0);
var chord = Demand(
	trig,
	0,
	Dbufrd(buf, count + 1.to(length), 1)
).reversed <! Demand(
	trig,
	0,
	Dbufwr(buf, count, note, 1)
);
var chordCps = chord.MidiCps;
var cf = Vibrato(chordCps, 6, 0.02, 0, 0, 0.04, 0.1, 0, 0);
var mf = chordCps * LinLin(LfPulse(1 / 8, 0, 0.5), 0, 1, 1.01, 2.01);
var sig = PmOsc(cf, mf, XLine(trig, 3, 0.0001, 0.2), 0);
var cmp = (sig * AmpCompA(chordCps, 0, 0.32, 1) * amp).sum;
XFade2(
	[cmp, cmp],
	GVerb(Bpf(cmp, 90.MidiCps, 1), 50, 8, 0.5, 0.5, 15, 0, 0.7, 0.5, 300),
	0.2,
	1
)
