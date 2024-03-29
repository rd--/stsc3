"Dseq ; at control rate"
| n x t f |
n := Dseq repeats: 3 list: #(1 3 2 7 8).
x := MouseX minval: 1 maxval: 40 warp: 1 lag: 0.1.
t := Impulse freq: x phase: 0.
f := (Demand trig: t reset: 0 demandUGens: n) * 30 + 340.
SinOsc freq: (f kr) phase: 0 mul: 0.1

"Dseq ; at audio rate"
| n x t f |
n := Dseq repeats: Float infinity list: #(1 3 2 7 8 32 16 18 12 24).
x := MouseX minval: 1 maxval: 10000 warp: 1 lag: 0.1.
t := Impulse freq: x phase: 0.
f := (Demand trig: t reset: 0 demandUGens: n) * 30 + 340.
SinOsc freq: f phase: 0 mul: 0.1

"Dseq ; the Sc2 Sequencer Ugen is somewhat like the sequ function below"
| sequ t n0 n1 |
sequ := [ :s :tr | Demand trig: tr reset: 0 demandUGens: (Dseq repeats: Float infinity list: s) ].
t := Impulse freq: 6 phase: 0.
n0 := sequ value: #(60 62 63 58 48 55) value: t.
n1 := sequ value: #(63 60 48 62 55 58) value: t.
LfSaw freq: {n0. n1} midiCps iphase: 0 mul: 0.05

"Dseq ; rather than Mce expansion at tr, it can be clearer to view tr as a functor"
| m |
m := [ :f |
	| tr sq |
	tr := Impulse freq: f phase: 0.
	sq := Dseq repeats: Float infinity list: #(60 63 67 69).
	(Demand trig: tr reset: 0 demandUGens: sq) midiCps kr
].
(SinOsc freq: (#(2 3 5) collect: m) phase: 0 mul: 0.1) splay2
