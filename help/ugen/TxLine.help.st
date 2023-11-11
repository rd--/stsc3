"TxLine ; exponential ramp from 220 to 440 over 5 seconds, every 10 seconds"
SinOsc freq: (TxLine start: 220 end: 440 dur: 5 trig: (Impulse freq: 0.1 phase: 0)) phase: 0 mul: 0.1
