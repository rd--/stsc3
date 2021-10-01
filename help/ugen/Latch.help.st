"Latch"
| freq |
freq := Latch in: (SinOsc freq: 13.3 phase: 0) trig: (Impulse freq: 10 phase: 0) mul: 200 add: 300.
SinOsc freq: freq phase: 0 mul: 0.2
