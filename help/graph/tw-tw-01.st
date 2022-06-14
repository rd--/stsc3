"<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)"
| s z |
s := [ :k |
	| f |
	f := [ :i |
		| x |
		x := Impulse freq: (k == 0 ifTrue: [ 1 ] ifFalse: [ 0.5 ** i / k ]) phase: 0.
		SinOsc freq: i phase: (SinOsc freq: ((i + k) ** i) phase: 0) / (Decay in: x decayTime: {i. i + 1} mul: k) kr ].
	(1 to: 6 collect: f) sum ].
z := (1 to: 16 collect: s) sum.
(GVerb in: z roomsize: 1 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) / 512
