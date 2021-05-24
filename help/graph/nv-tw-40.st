"http://sccode.org/1-V (nv) [Line 40] ; minus freeverb2"
| nd s |
nd := [:i |
 | t f |
 t := Impulse freq: (2 raisedTo: i) / 32 phase: 1 / 2 mul: (0.6 raisedTo: i) * 40.
 f := (4 constant raisedTo: (LFNoise0 freq: 1 / 16)) * 300.
 (RLPF in: t freq: f kr rq: 0.005) sin].
Splay input: (8 mceFillZeroIndexed: nd) * 0.25
