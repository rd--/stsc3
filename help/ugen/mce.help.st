"mce ; multiple channel expansion ; array literal"
SinOsc freq: #(440 441) phase: 0 mul: 0.1

"mce ; annotated"
SinOsc freq: #(440 441) mce phase: 0 mul: 0.1

"mce ; multiple channel expansion ; array with"
SinOsc freq: {220. 220 * 2} phase: 0 mul: 0.1

"mce ; two-level ; 220 & 221 at left ; 880 & 881 at right"
(SinOsc freq: #(#(220 880) #(221 881)) phase: 0 mul: 0.1) mix

"mce ; two-level ; transpose ; 220 & 880 at left ; 221 & 881 at right"
(SinOsc freq: #(#(220 880) #(221 881)) mce transpose phase: 0 mul: 0.1) mix

"mce ; indexed ; single channel ; 440hz (one-indexed)"
SinOsc freq: (#(55 110 220 440 880) mce at: 4) phase: 0 mul: 0.1
