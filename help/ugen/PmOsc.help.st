"PmOsc ; composite of SinOsc, ie. SinOsc r cf (SinOsc r mf mp * pm) ; modulate carfreq"
(PmOsc carfreq: (Line start: 600 end: 900 dur: 5 doneAction: 0) modfreq: 600 pmindex: 3 modphase: 0) * 0.1

"PmOsc ; composite of SinOsc, ie. SinOsc r cf (SinOsc r mf mp * pm) ; modulate modfreq"
(PmOsc carfreq: 300 modfreq: (Line start: 600 end: 900 dur: 5 doneAction: 0) pmindex: 3 modphase: 0) * 0.1

"PmOsc ; modulate index"
(PmOsc carfreq: 300 modfreq: 550 pmindex: (Line start: 0 end: 20 dur: 8 doneAction: 0) modphase: 0) * 0.1
