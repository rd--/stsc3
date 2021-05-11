"<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters) ; "
| nd |
nd := [:k |
 | x y |
 x := [:f :m :j |
  SinOsc freq: f + (m * 4 * j) phase: m mul: (LFNoise1 freq: j constant + 1 / f * 4) / 2].
 y := x value: 0.1 value: 0 value: 8.
 0 to: 3 do: [:i | y := x value: #(4 8) mce * i * k value: y value: i]. "<- simplified 9->4 because of hs rewriter"
 y].
(8 mixFill: nd) / 4
