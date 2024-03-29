"mce ; multiple channel expansion ; array literal"
SinOsc freq: #(440 441) phase: 0 mul: 0.1

"mce ; multiple channel expansion ; array with"
SinOsc freq: {220. 220 * 2} phase: 0 mul: 0.1

"mce ; two-level ; 220 & 221 at left ; 880 & 881 at right"
(SinOsc freq: #(#(220 880) #(221 881)) phase: 0 mul: 0.1) sum

"mce ; two-level ; transpose ; 220 & 880 at left ; 221 & 881 at right"
(SinOsc freq: #(#(220 880) #(221 881)) transpose phase: 0 mul: 0.1) sum

"mce ; indexed ; single channel ; 440hz (one-indexed)"
SinOsc freq: (#(55 110 220 440 880) at: 4) phase: 0 mul: 0.1

"mce ; clump ; 220 & 880 at left ; 221 & 881 at right"
(SinOsc freq: (#(220 221 880 881) groupsOf: 2) phase: 0 mul: 0.1) sum
