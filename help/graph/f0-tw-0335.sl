;; https://sccode.org/1-4Qy ; f0 ; 0335
var o = GrainFM(
	1,
	LFSaw([0.5, 0.6], 0),
	16,
	LFSaw(5, 0) * LFSaw(0.015, 0) + 1 * 98,
	(2 ** LFSaw(4, 0)).roundTo(0.5) * 99,
	2 ** LFSaw(1 / [8, 9], 0) * 8,
	0,
	-1,
	512
);
(o / 2).tanh

;; https://sccode.org/1-4Qy ; f0 ; 0335 ; with keywords
var o = GrainFM(
	numChan: 1,
	trigger: LFSaw([0.5, 0.6], 0),
	dur: 16,
	carfreq: LFSaw(5, 0) * LFSaw(0.015, 0) + 1 * 98,
	modfreq: (2 ** LFSaw(4, 0)).roundTo(0.5) * 99,
	index: 2 ** LFSaw(1 / [8, 9], 0) * 8,
	pan: 0,
	envbufnum: -1,
	maxGrains: 512
);
(o / 2).tanh
