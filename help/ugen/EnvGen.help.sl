(* EnvGen ; curve=3=sin *)
var trg = Impulse(1 / 9, 0);
var env = Env([0, 0.1, 0.1, 0], [3, 2, 3], 3, nil, nil, 0);
SinOsc(440, 0) * EnvGen(trg, 1, 0, 1, 0, env.asArray)

(* EnvGen ; if gate < 0 then the envelope will end immediately with release time set to 0 - gate + 1 *)
var gate = LinLin(LfPulse(0.25, 0, 0.1), -1, 1, MouseX(-25, 0, 0, 0.2), 1);
PinkNoise() * Asr(gate, 0.01, 1, -4) * 0.1

(* EnvGen ; https://scsynth.org/t/6348/3 *)
var ln = [
	75 27 3.0;
	66 26 3.5;
	50 21 3.0;
	90 21 4.0
];
var trig = Impulse(1 / [4, 5], 0);
var pairs = DmdFor(trig, 0.0, Drand(inf, ln));
var env = Env(
	[pairs.first, pairs.first, pairs.second],
	[0, pairs.third],
	[-4],
	nil,
	nil,
	0
).asEnvGen(trig);
SinOsc(env.kr * 2, 0) * 0.25
