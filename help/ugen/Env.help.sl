;; Env ; asEnvGen
var e = Env([440, 440, 324, 10000], [4, 2, 1], ['exp'], nil, nil, 0);
var f = e.asEnvGen;
SinOsc(f, 0) * 0.1

;; Env ; asEnvGen:withDoneAction:
var e = Env([440, 440, 324, 10000], [4, 2, 1], ['exp'], nil, nil, 0);
var f = e.asEnvGen(1, withDoneAction: 2);
SinOsc(f, 0) * 0.1

;; Env ; EnvGen
var e = Env([440, 440, 324, 10000], [4, 2, 1], ['exp'], nil, nil, 0);
var f = EnvGen(1, 1, 0, 1, 2, e);
SinOsc(f, 0) * 0.1

;; Env ; keywords
var e = Env(levels: [440, 440, 324, 10000], times: [4, 2, 1], curves: ['exp'], releaseNode: nil, loopNode: nil, offset: 0);
var f = EnvGen(gate: 1, levelScale: 1, levelBias: 0, timeScale: 1, doneAction: 2, envelope: e);
SinOsc(freq: f, phase: 0) * 0.1
