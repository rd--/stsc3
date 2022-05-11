"raisedTo ; **"
| a |
a := SinOsc freq: 100 phase: 0 * 0.1.
{a. a raisedTo: 10}

"----"
(SinOsc new ** 1.1) draw
(1.1 ** SinOsc new) draw
2 ** #(1 2 3) "=> (2 4 8)"
2.5 ** #(1 2 3) "=> (2.5 6.25 15.625)"
