(* Env ; asEnvGen *)
var levels = [440 440 324 10000];
var times = [4 2 1];
var curves = 2;
var env = Env(levels, times, curves, nil, nil, 0);
var freq = env.asEnvGen(1);
SinOsc(freq, 0) * 0.1

(* Env ; EnvGen *)
var e = Env([440 440 324 10000], [4 2 1], 2, nil, nil, 0);
var f = EnvGen(1, 1, 0, 1, 2, e.asArray);
SinOsc(f, 0) * 0.1

(* Env ; circle *)
var env = Env([6000 700 100], [1 1], ['exp', 'lin'], nil, nil, 0).circle(0, 'lin');
SinOsc(
	env.asEnvGen(1),
	0
) * 0.1 + Impulse(1, 0)

(* Env ; circle *)
var env = Env([6000 700 100], [1 1], ['exp', 'lin'], nil, nil, 0).circle(1, 'lin');
SinOsc(
	env.asEnvGen(MouseX(-1, 1, 0, 0.2)),
	0
) * 0.1 + Impulse(1, 0)

(* ---- Env ; requires=keywords *)
var e = Env(
	levels: [440, 440, 324, 10000],
	times: [4, 2, 1],
	curves: 2,
	releaseNode: nil,
	loopNode: nil,
	offset: 0
);
var f = EnvGen(
	gate: 1,
	levelScale: 1,
	levelBias: 0,
	timeScale: 1,
	doneAction: 2,
	envelope: e.asArray
);
SinOsc(
	freq: f,
	phase: 0
) * 0.1

(* ---- Env ; asEnvGenWithDoneAction *)
var e = Env([440, 440, 324, 10000], [4, 2, 1], 2, nil, nil, 0);
var f = e.asEnvGenWithDoneAction(1, 2);
SinOsc(f, 0) * 0.1
