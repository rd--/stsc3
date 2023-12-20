"https://twitter.com/redFrik/status/1319946903711338496 ; graph rewrite (rd)"
| scl gen |
scl := #(0 0.804 1.944 3.084 3.888 5.028 5.832 6.972 7.776 8.916 10.056 10.86).
gen := [ :tr |
	| i z |
	i := PulseCount trig: tr reset: 0.
	z := [
		| f y o n m |
		o := 12 * (TRand lo: 3 hi: 6 trig: tr).
		n := (Select which: (TChoose trig: tr array: #(0 2 3 5 7 8 10)) array: scl).
		m := (o + n).
		f := LfTri freq: 1 / 99 iphase: 0 mul: 9 add: m midiCps.
		y := SinOscFb freq: f feedback: ((LfTri freq: (1 + i) / 50 iphase: 0) + 1) / 2.
		Pan2 in: y pos: (LfTri freq: i / 70 iphase: 0) level: ((TRand lo: 0 hi: 1 trig: tr) > 0.5 / 2)
	] dup: 9.
	(CombN in: z sum maxdelaytime: 0.2 delaytime: 0.2 decaytime: 1) tanh * 0.1
].
OverlapTexture graphFunc: gen sustainTime: 3 transitionTime: 9 overlap: 2
