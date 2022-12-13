"Splay2 ; 8 -> 2"
([ SinOsc freq: 500 + 500 rand phase: 0 mul: 0.05 ] dup: 8) splay2

"Splay2 ; 8 -> 2"
Splay2 inArray: ([ SinOsc freq: 500 + 500 rand phase: 0 mul: 0.05 ] dup: 8)

"Splay2 ; 3 -> 2"
Splay2 inArray: ([ SinOsc freq: 500 + 500 rand phase: 0 mul: 0.1 ] dup: 3)

"Splay2 ; 4 -> 2"
Splay2 inArray: ([ SinOsc freq: 500 + 500 rand phase: 0 mul: 0.1 ] dup: 4)

"Splay2 ; 5 -> 2"
Splay2 inArray: ([ SinOsc freq: 500 + 500 rand phase: 0 mul: 0.1 ] dup: 5)
