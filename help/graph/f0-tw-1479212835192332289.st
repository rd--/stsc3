"https://twitter.com/redFrik/status/1479212835192332289 (f0)"
| q q1 b q2 e q3 q4 q5 q6 |
q := [ :freq | QuadC freq: freq a: 1 b: -1 c: -0.75 xi: 0 ].
q1 := q value: 5.
b := #(5 2.5 6).
q2 := (q value: b / 15) * q1.
e := 1 / 15.
q3 := (q value: 500) * q1 / 99.
q4 := (q value: 999 * b) * ((q value: b / 15) max: 0) / 999 * (q value: b).
q5 := (q value: #(12 16) * 99) * ((QuadC freq: 0.1 a: 1 b: -1 c: -0.75 xi: #(-0.5 -1)) max: 0) * ((q value: 15) max: 0) / 15.
q6 := (Pan2 in: (QuadC freq: 99 * b a: 1 b: (q value: 1 / b / #(15 5 15)) > 0 * 0.375 - 1.25 c: -0.75 xi: 0) tanh pos: 0.5 * q2 level: q2) sum / 2.
q3 + q4 splay2 + (AllpassN in: q5 + q6 maxdelaytime: e delaytime: e decaytime: 15)
