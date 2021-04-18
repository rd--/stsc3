"<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)"
| s z |
s := [:k |
 | f |
 f := [:i |
  | x |
  x := Impulse freq: (k == 0 ifTrue: [1.0] ifFalse: [(0.5 raisedTo: i) / k]) phase: 0.
  SinOsc freq: i phase: ((SinOsc freq: ((i + k) raisedTo: i asFloat) phase: 0) / (Decay in: x decayTime: {i. i + 1} mul: k) kr)].
 6 mixFillZeroIndexed: f].
z := 16 mixFillZeroIndexed: s.
((GVerb in: z roomsize: 1 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.0) / 512)
