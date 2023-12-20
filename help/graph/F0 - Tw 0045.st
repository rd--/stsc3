"http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)"
| f pf p |
f := SinOsc freq: (SinOsc freq: 0.11 phase: 0) phase: 0.
pf := SinOsc
	freq: ((SinOsc freq: 0.01 phase: 0) + 1) * 95
	phase: 0
	mul: (SinOsc freq: 0.005 phase: 0 mul: 50)
	add: 100.
p := SinOsc
	freq: pf
	phase: (SinOsc freq: #(98 97) phase: 0)
	mul: (SinOsc freq: 0.0005 phase: 0) + Float pi.
(SinOsc freq: f phase: p) tanh * 0.1
