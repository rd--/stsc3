(* LinGen *)
var env = LinGen(1, [2, 0.5], [1]);
SinOsc(440 * env, 0) * 0.1

(* LinGen *)
var voiceFunc = { :e |
	var freq = LinGen(e.w, e.p.unitCps * [2, 0.5], [1]);
	SinOsc(freq, 0) * e.z * e.w
};
Voicer(16, voiceFunc).sum

(* LinGen *)
var env = LinGen(Impulse(4, 0), [0, 1, 1, 0], [0.001, 0.05, 0.1]);
SinOsc(440 * env, 0) * 0.1
