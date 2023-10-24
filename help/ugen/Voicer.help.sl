(* SinOsc ; event control ; p-field *)
Voicer(16) { :e |
	SinOsc(e.p.UnitCps, 0) * e.w * e.z
}.Splay2

(* SinOsc ; event control ; x-field *)
Voicer(16) { :e |
	EqPan2(SinOsc((e.x * 24 + 48).MidiCps, 0), e.i * 2 - 1) * e.z * e.w
}.sum

(* Blip ; event control ; shared parameter *)
var nh = 1;
Voicer(16) { :e |
	Blip(e.p.UnitCps, nh) * e.w * e.z
}.Splay2

(* Blip ; event control *)
Voicer(16) { :e |
	EqPan2(Blip(e.p.UnitCps, e.y * 10 + 1), e.i * 2 - 1) * e.w * e.z * e.z
}.sum

(* PanAz ; event control *)
Voicer(16) { :e |
	PanAz(24, Blip(e.p.UnitCps, e.y * 3 + 1), e.i * 2 - 1, 1, 3, 0) * e.w * e.z * e.z
}.sum

(* dictionary ; voicer *)
(
	w: SinOsc(1 / [5, 7], 0),
	x: SinOsc(1 / [13, 17], 0)
).Voicer { :e |
	SinOsc(e.x * 222 + 333, 0) * e.w * 0.2
}

(* blip ; event control ; keywords *)
var f = { :e |
	EqPan2(
		in: Blip(freq: e.p.UnitCps, numharm: e.y * 10 + 1),
		pos: e.i * 2 - 1
	) * e.w * e.z * e.z
};
Voicer(numVoices: 16, voiceFunc: f).Sum

(* MembraneCircle ; event control ; note limited voice count ; keywords *)
var f = { :e |
	var loss = LinExp(in: e.y, srclo: 0, srchi: 1, dstlo: 0.99999, dsthi: 0.99950);
	var wobble = SinOsc(freq: 2, phase: 0);
	var tension = LinExp(in: e.x, srclo: 0, srchi: 1, dstlo: 0.01, dsthi: 0.1) + (wobble * 0.0001);
	var env = Perc(trig: e.w, attackTime: 0.0001, releaseTime: 1 - e.z, curve: -4) * (e.z + (e.y / 4));
	Pan2(in: MembraneCircle(excitation: PinkNoise() * env, tension: tension, loss: loss), pos: e.i * 2 - 1, level: 1)
};
Voicer(numVoices: 6, voiceFunc: f).Sum

(* pluck ; event control ; keywords *)
var f = { :e |
	var n = WhiteNoise() * e.z * 2;
	var dlMax = 1 / 220;
	var dl = dlMax * (1 - e.x * 0.9);
	EqPan2(
		in: Pluck(in: n, trig: e.w, maxdelaytime: dlMax, delaytime: dl, decaytime: 10, coef: e.y / 3),
		pos: e.i * 2 - 1
	)
};
Voicer(numVoices: 16, voiceFunc: f).Sum

(* resonz ; pinkNoise ; event control ; keywords *)
var f = { :e |
	EqPan2(
		in: Resonz(in: PinkNoise(), freq: (e.x * 24 + 48).MidiCps, bwr: e.y * 0.25) * 24,
		pos: e.i * 2 - 1
	) * e.z.Squared * e.w
};
Voicer(numVoices: 16, voiceFunc: f).Sum
