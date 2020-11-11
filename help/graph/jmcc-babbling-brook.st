"babbling brook (jmcc) #SC3"
b := [:f :m :a :g |
 [|n1 n2|
  n1 := OnePole in: (BrownNoise new) coef: 0.99 .
  n2 := (LPF in: (BrownNoise new) freq: f) * m + a .
  (RHPF in: n1 freq: n2 rq: 0.03) * g]] .
x := b value: 14 value: 400 value: 500 value: 0.06 .
y := b value: 20 value: 800 value: 1000 value: 0.10 .
o := (MCE source: {x value . x value}) + (MCE source: {y value . y value}) .
o
