"f0 ; <https://twitter.com/redFrik/status/1395878538297892865>"
| t g e m o c |
t := Impulse freq: 5 phase: 0.
g := #(3 2).
e := (LagUd in: t lagTimeU: 0.001 lagTimeD: ((SinOscFb freq: g / 99 feedback: 0) + 1.08)).
m := (#(24 0 3 5 7 10 36) collect: [ :i | i + #(36 48 36 33 60 72) ]) concatenation.
o := SinOscFb
	freq: (DmdOn trig: t reset: 0 demandUGens: (Dseq repeats: Float infinity list: m midiCps) / g)
	feedback: (SinOscFb freq: 0.02 feedback: 0)
	mul: e.
c := (Rlpf in: o freq: (((3 ** (SinOscFb freq: 0.04 feedback: 0)) + e) * 2000) rq: ((3 ** (SinOscFb freq: (g / 9) feedback: 0)) / 3)) * 4.
((CombC in: {c tanh. c} maxdelaytime: 1 delaytime: 1 / #(2 3) decaytime: #(4 5)) sum / 8 + c) tanh * 0.1
