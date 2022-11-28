"moto rev (jmcc) #1"
| f s |
f := SinOsc freq: 0.2 phase: 0 mul: 10 add: 21.
s := LfPulse freq: f kr iphase: #(0 0.1) width: 0.1.
(Rlpf in: s freq:100 rq: 0.1) clip2: 0.4
