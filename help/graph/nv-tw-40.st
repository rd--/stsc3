"http://sccode.org/1-V (nv) [Line 40] ; minus freeverb2"
| nd s |
nd := [ :i |
	| t f |
	t := Impulse freq: (2 ** i) / 32 phase: 1 / 2 mul: (0.6 ** i) * 40.
	f := (4 ** (LfNoise0 freq: 1 / 16)) * 300.
	(Rlpf in: t freq: f kr rq: 0.005) sin
].
(0 to: 7 collect: nd) splay2 * 0.25
