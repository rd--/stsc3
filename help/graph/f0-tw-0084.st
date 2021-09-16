"http://sccode.org/1-4Qy (f0)"
| f g w |
f := Saw freq: ((5 to: 15 by: 2) * 19) mul: 99 add: 199.
g := Saw freq: ((1 to: 13 by: 3) * 29) mul: 199 add: 299.
w := Saw freq: (Saw freq: 3 mul: 2 add: 3) * (5 to: 15 by: 2) * 19 mul: 299 add: 399.
((Formant fundfreq: f formfreq: g bwfreq: w) splay) / 9
