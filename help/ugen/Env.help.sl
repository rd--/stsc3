(* Env ; asEnvGen *)
var e = Env([440 440 324 10000], [4 2 1], 2, nil, nil, 0);
var f = e.asEnvGen(1);
SinOsc(f, 0) * 0.1

(* Env ; EnvGen *)
var e = Env([440 440 324 10000], [4 2 1], 2, nil, nil, 0);
var f = EnvGen(1, 1, 0, 1, 2, e.asArray);
SinOsc(f, 0) * 0.1

(* Env ; requires=keywords *)
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
