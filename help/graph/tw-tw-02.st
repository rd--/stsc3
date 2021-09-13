"<https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)"
| depth nd |
depth := 3. "<- initially simplified from 8 because of hs rewriter, however 3 also sounds well"
nd := [:k |
 | x y |
 x := [:f :m :j |
  SinOsc freq: f + (m * 4 * j) phase: m mul: (LFNoise1 freq: j + 1 / f * 4) / 2].
 y := x value: 0.1 value: 0 value: 8.
 0 to: depth do: [:i | y := x value: #(4 8) asUGen * i * k value: y value: i]. "<- divide by zero..."
 y].
((8 mixFill: nd) / 4)
