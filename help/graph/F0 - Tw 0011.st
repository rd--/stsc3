"tw 0011 (f0) ; https://twitter.com/redFrik/status/23182604046"
| s f |
s := [ :o :i |
	SinOsc
		freq: (i + #(0 0.0001) ** 2) * (f value: o value: i - 1)
		phase: (f value: o value: (i - 1)) * 0.0001
		mul: (f value: o value: i - 1)].
f := [ :o :i | i > 0 ifTrue: [ s value: o value: i ] ifFalse: [ o ] ].
(f value: 60 value: 6) / 60
