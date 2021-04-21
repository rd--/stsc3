"http://sccode.org/1-4Qy (f0)"
| f g w o |
f := Saw freq: ((Interval from: 5 to: 15 by: 2) asArray * 19) mul: 99 add: 199.
g := Saw freq: ((Interval from: 1 to: 13 by: 3) asArray * 29) mul: 199 add: 299.
w := Saw freq: (Saw freq: 3 mul: 2 add: 3) * (Interval from: 5 to: 15 by: 2) asArray * 19 mul: 299 add: 399.
o := Formant fundfreq: f formfreq: g bwfreq: w.
(Splay input: o spread: 1 level: 1 center: 0 levelComp: true) / 9
