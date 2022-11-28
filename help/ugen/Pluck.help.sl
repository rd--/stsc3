;; Pluck ; https://scsynth.org/t/4318/17
var freq = LfNoise2(1);
var trig = Impulse(LinExp(freq, -1, 1, 1, 100), 0);
var freqs = (60.5 + [0, 2, 4, 5, 7, 9, 10]).MidiCps;
var snd = Pluck(Hasher(Sweep(trig, 1)) * -10.dbAmp, trig, 1 / freqs, 1 / freqs, 0.9, 0.5);
snd := LeakDc(snd, 0.995).sum;
snd := MoogFf(snd, LinExp(LfNoise2(1), -1, 1, 500, 16000), 0, 0);
Pan2(snd, freq, 1)

;; Pluck ; event control ; requires=voicer
Voicer(16, { :e |
	var dm = 1 / 220;
	var dl = (e.x.negated * 0.9 + 1) * dm;
	var sig = Pluck(WhiteNoise() * e.z, e.w, dm, dl, 10, e.y / 3);
	Pan2(sig, e. o * 2 - 1, 1)
}).sum
