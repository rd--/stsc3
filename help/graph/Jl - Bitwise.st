"bitwise ; a0f253ff89f6b244ea29a1e431dd9e5df5571a8b (jl)"
| t p1 p2 p3 p4 s |
t := PulseCount trig: (Impulse freq: 8000 phase: 0) reset: 0.
p1 := (t * 15) bitAnd: (t shiftRight: 5).
p2 := (t * 5) bitAnd: (t shiftRight: #(3 4)).
p3 := (t * 2) bitAnd: (t shiftRight: 9).
p4 := (t * 8) bitAnd: (t shiftRight: 11).
s := ({p1. p2. p3. p4} inject: 0 into: [:i :j | j bitOr: i]) - 3 % 256.
(Hpf in: (((s / 127) - 1) * 3) freq: 20) tanh * 0.02
