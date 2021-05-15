"http://sccode.org/1-V (nv) [Line 41]"
| nd s |
nd := [:i |
 | t f |
 t := Impulse freq: (2 constant ** i) / 32 phase: 1/2 mul: (0.6 constant ** i) * 40.
 f := 4 constant ** (LFNoise0 freq: 1/16) * 300.
 (RLPF in: t freq: f rq: 0.005) sin].
Splay input: (7 mceFill: nd) spread: 1 level: 1 center: 0 levelComp: true
