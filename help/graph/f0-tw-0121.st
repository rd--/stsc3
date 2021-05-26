"http://sccode.org/1-4Qy (f0)"
| z l m o |
z := SinOsc freq: {1/8. 1/7} phase: 0 mul: (SinOsc freq: (1/30) phase: 0 mul: 9).
l := (Interval from: 56 to: 98 by: 6) collect: [:x | x midicps].
m := SinOsc freq: {1/4. 1/3} phase: 0.
o := SinOsc freq: (Select which: z array: l) kr phase: 0 mul: m.
(CombN in: o maxdelaytime: 1 delaytime: {1/6. 1/5} decaytime: 9) tanh * 0.1
