"mod"
(SinOsc freq: 100 phase: 4) % 1 * 0.1

"----"
5 iota collect: [:x| x rem: 2] "=> (1 0 1 0 1)"
5 iota collect: [:x| x quo: 2] "=> (0 1 1 2 2)"
5 iota collect: [:x| x // 2] "=> (0 1 1 2 2)"
