| e |
e := EventParam fromUGen: #(1 2 3 4 5 6 7 8 9) mce.
SinOsc freq: (e y * 110) phase: 0 mul: 0.1
