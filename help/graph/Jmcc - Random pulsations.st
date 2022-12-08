"random pulsations (jmcc) #1 ; graph rewrite"
| f |
f := [ :tr |
	| o1 o2 o3 |
	o1 := FSinOsc freq: (TRand lo: 0 hi: 2000 trig: tr) iphase: 0.
	o2 := SinOsc freq: (TRand lo: 8 hi: 88 trig: tr) phase: 0.
	o3 := SinOsc freq: (0.3 + (TRand lo: 0 hi: 0.5 trig: tr)) phase: (TRand lo: 0 hi: Float pi * 2 trig: tr) mul: 0.7.
	Pan2 in: (o1 amClip: o2) pos: o3 kr level: 0.05 ].
OverlapTexture graphFunc: f sustainTime: 5 transitionTime: 2 overlap: 6
