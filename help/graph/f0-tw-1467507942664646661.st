"https://twitter.com/redFrik/status/1467507942664646661"
| b fb1 d c p q |
b := #(3 4 5 6 7 8 1 2).
fb1 := [ :freq | FBSineC freq: freq im: 1 fb: 0.1 a: 1.1 c: 0.5 xi: 0.1 yi: 0.1 ].
d := (fb1 value: b / 12) > 0 / 80 + 1.051.
c := (fb1 value: 1 / b) + 1.
p := FBSineC
	freq: 999 * b
	im: 2 ** (fb1 value: b / 9) * 4 - 1
	fb: (fb1 value: b / 8) / 2 + 1 + (99 ** (fb1 value: 1 / 4) / 99)
	a: d
	c: (fb1 value: 1 / b) / 40 + 0.1
	xi: 0.1
	yi: 0.1.
q := FBSineC freq: (fb1 value: 27 + b) > 0 + 2 + b * d * 1400 im: 2 fb: c a: c / 2 c: 2 xi: 2 yi: 0.1.
(3 ** (fb1 value: 1 / b) / 5 * p + (q / 12) + ((HPF in: (fb1 value: 32) < 0 freq: 3) * (fb1 value: (b * 70 / d)) * (fb1 value: 1 / 2))) splay2 / 2
