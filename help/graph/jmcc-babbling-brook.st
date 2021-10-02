"babbling brook (jmcc) #SC3"
| b |
b := [ :f :m :a :g |
  | n1 n2 |
  n1 := OnePole in: BrownNoise new coef: 0.99.
  n2 := LPF in: BrownNoise new freq: f mul: m add: a.
  RHPF in: n1 freq: n2 rq: 0.03 mul: g ].
[ b value: 14 value: 400 value: 500 value: 0.06 ] dup + [ b value: 20 value: 800 value: 1000 value: 0.10 ] dup
