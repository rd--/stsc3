"f0 ; https://twitter.com/redFrik/status/839296011982024704"
| t f m i g |
t := LfSaw freq: #(0.5 0.6) iphase: 0.
f := (LfSaw freq: 5 iphase: 0 mul: (LfSaw freq: 0.015 iphase: 0) add: 1) * 98.
m := (2 ** ((LfSaw freq: 4 iphase: 0) roundTo: 0.5)) * 99.
i := (2 ** (LfSaw freq: 1 / #(8 9) iphase: 0)) * 8.
g := GrainFm
 	numChan: 1
	trigger: t
	dur: 16
	carfreq: f
	modfreq: m
	index: i
	pan: 0
	envbufnum: -1
	maxGrains: 512.
(g / 2) tanh * 0.25
