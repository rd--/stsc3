"f0 ; <https://twitter.com/redFrik/status/1395519538008141835>"
| c d f p o |
c := DmdFor dur: 0.004 reset: 0 level: (Dseries length: Float infinity start: 1 step: #(1 2)) % (DmdFor dur: 8.192 reset: 0 level: (Seq repeats: Float infinity list: ((1 to: 6) * 75))).
d := Hpf in: (MantissaMask in: c bits: 3) freq: 5.
f := [ :x | (SinOscFb freq: x feedback: 0) max: 0 ].
p := #(250 200) * ((Duty dur: 4.096 reset: 0 doneAction: 0 level: (Dseq repeats: Float infinity list: #(4 6 5 5 5 5 3) / 4)) lag: 0.1).
o := ((SinOscFb freq: p feedback: (f value: 0.08)) * (f value: 1 / #(99 60))) / 3.
(Lpf in: ((((d sin + ((SinOscFb freq: 63 feedback: (f value: 0.8)) * d)) / 9) + o) tanh / 2) freq: 10000) * 0.1
