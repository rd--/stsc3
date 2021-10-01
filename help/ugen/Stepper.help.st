"Stepper"
| freq |
freq := Stepper trig: (Impulse freq: 10 phase: 0) reset: 0 min: 1 max: 10 step: 1 resetval: 1 mul: 100.
SinOsc freq: freq phase: 0 mul: 0.1
