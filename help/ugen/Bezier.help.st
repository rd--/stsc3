"Bezier - reference sine tone"
SinOsc freq: 440 phase: 0 mul: 0.1

"Bezier - Bézier curve oscillator ; bezier approximation of sin function"
Bezier haltAfter: 100 dx: 0.0001 freq: 440 phase: 0 param: #(0 0 0.2 -1.3 0.3 -1.3 0.5 0 0.7 1.3 0.8 1.3 1 0) mul: 0.1

"Bezier - mouse control of frequency"
| x |
x := MouseX minval: 40 maxval: 4000 warp: 1 lag: 0.2.
Bezier haltAfter: 100 dx: 0.0001 freq: x phase: 0 param: #(0 0 0.2 -1.3 0.3 -1.3 0.5 0 0.7 1.3 0.8 1.3 1 0) mul: 0.1

"Bezier - mouse control of frequency & one control point"
| x y |
x := MouseX minval: 40 maxval: 4000 warp: 1 lag: 0.2.
y := MouseY minval: 0.1 maxval: 0.2 warp: 0 lag: 0.2.
Bezier haltAfter: 100 dx: 0.0001 freq: x phase: 0 param: {0. 0. y. -1.3. 0.3. -1.3. 0.5. 0. 0.7. 1.3. 0.8. 1.3. 1. 0} mul: 0.1

"Bezier - dynamic shifting of control points as oscillator"
| w f |
w := [ :rng | LinLin in: (LfDNoise3 freq: 2) srclo: -1 srchi: 1 dstlo: rng key dsthi: rng value ].
f := [ :ix |
	| p o |
	p := {0. 0. w value: 0.05 -> 0.25. -1.3. w value: 0.25 -> 0.45. -1.3. 0.5. 0. w value: 0.55 -> 0.75. 1.3. w value: 0.75 -> 0.95. 1.3. 1. 0}.
	o := Bezier haltAfter: 100 dx: 0.0001 freq: (w value: 200 -> 205) phase: 0 param: p.
	Pan2 in: o pos: (IRand lo: -1 hi: 1) level: 0.1
].
((1 to: 4) collect: f) sum
