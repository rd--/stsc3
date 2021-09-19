"pmOsc ; composite of sinOsc, ie. sinOsc r cf (sinOsc r mf mp * pm) ; modulate carfreq"
PMOsc carfreq: (Line start: 600 end: 900 dur: 5 doneAction: 0) modfreq: 600 pmindex: 3 modphase: 0 mul: 0.1

"pmOsc ; composite of sinOsc, ie. sinOsc r cf (sinOsc r mf mp * pm) ; modulate modfreq"
PMOsc carfreq: 300 modfreq: (Line start: 600 end: 900 dur: 5 doneAction: 0) pmindex: 3 modphase: 0 mul: 0.1

"pmOsc ; modulate index"
PMOsc carfreq: 300 modfreq: 550 pmindex: (Line start: 0 end: 20 dur: 8 doneAction: 0) modphase: 0 mul: 0.1
