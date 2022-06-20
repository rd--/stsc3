"OverlapTexture ; random panning sines"
| f |
f := [ :tr |
	Pan2 in: (SinOsc freq: (TRand lo: 10 hi: 2000 trig: tr) phase: 0) pos: (TRand lo: -1 hi: 1 trig: tr) level: 0.02
].
OverlapTexture graphFunc: f sustainTime: 2 transitionTime: 5 overlap: 12
