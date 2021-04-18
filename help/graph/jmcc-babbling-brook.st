"babbling brook (jmcc) #SC3"
| b o x y |
b := [:d |
 [| n1 n2 |
  n1 := OnePole in: (BrownNoise new) coef: 0.99.
  n2 := LPF in: (BrownNoise new) freq: (d at: 1) mul: (d at: 2) add: (d at: 3).
  RHPF in: n1 freq: n2 rq: 0.03 mul: (d at: 4)]].
x := b value: #(14 400 500 0.06).
y := b value: #(20 800 1000 0.10).
{x value. x value} mce + {y value. y value} mce
