;; https://sccode.org/1-4Qy ; f0 ; 0335
var o = GrainFm(
	1,
	LfSaw([0.5, 0.6], 0),
	16,
	LfSaw(5, 0) * LfSaw(0.015, 0) + 1 * 98,
	(2 ** LfSaw(4, 0)).RoundTo(0.5) * 99,
	2 ** LfSaw(1 / [8, 9], 0) * 8,
	0,
	-1,
	512
);
(o / 2).Tanh

;; https://sccode.org/1-4Qy ; f0 ; 0335 ; with keywords
var o = GrainFm(
	numChan: 1,
	trigger: LfSaw([0.5, 0.6], 0),
	dur: 16,
	carfreq: LfSaw(5, 0) * LfSaw(0.015, 0) + 1 * 98,
	modfreq: (2 ** LfSaw(4, 0)).RoundTo(0.5) * 99,
	index: 2 ** LfSaw(1 / [8, 9], 0) * 8,
	pan: 0,
	envbufnum: -1,
	maxGrains: 512
);
(o / 2).Tanh
