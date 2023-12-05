(* Pluck ; https://scsynth.org/t/4318/17 *)
var freq = LfNoise2(1);
var trig = Impulse(LinExp(freq, -1, 1, 1, 100), 0);
var freqs = (60.5 + [0 2 4 5 7 9 10]).MidiCps;
var snd = Pluck(Hasher(Sweep(trig, 1)) * -10.DbAmp, trig, 1 / freqs, 1 / freqs, 0.9, 0.5);
snd := LeakDc(snd, 0.995).Sum;
snd := MoogFf(snd, LinExp(LfNoise2(1), -1, 1, 500, 16000), 0, 0);
EqPan(snd, freq)

(* Pluck ; event control ; requires=voicer *)
Voicer(16) { :e |
	var dm = 1 / 220;
	var dl = (e.x.negated * 0.9 + 1) * dm;
	var sig = Pluck(WhiteNoise() * e.z, e.w, dm, dl, 10, e.y / 3);
	EqPan2(sig, e.i * 2 - 1)
}.Mix

(* Pluck ; event control ; requires=record/voicer *)
(
	w: SinOsc(Rand((2 .. 9) / 11, 1), 0),
	x: LfNoise2(Rand((9 .. 2) / 13, 1)).Range(0, (2 .. 9) / 17),
	y: LfNoise2(Rand((2 .. 9) / 15, 2)).Range((9 .. 2) / 19, 1),
	z: LfNoise2(Rand(1, (9 .. 2) / 11)).Range(0, (2 .. 9) / 9)
).Voicer { :e |
	var dm = 1 / 220;
	var dl = (e.x.negated * 0.9 + 1) * dm;
	var sig = Pluck(WhiteNoise() * e.z, e.w, dm, dl, 10, e.y / 3);
	EqPan2(sig, e.y * 2 - 1) * 0.25
}.Mix
