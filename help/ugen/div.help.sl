;; Idiv ; integer division ; quotient
var d = (SinOsc(0.25, 0) * 200 + 500).Idiv(17);
SinOsc((d * 3).midiCps, 0) * 0.1

;; Idiv ; integer division ; quotient
var d = (SinOsc(0.25, 0) * 200 + 500) // 17;
SinOsc((d * 3).midiCps, 0) * 0.1
