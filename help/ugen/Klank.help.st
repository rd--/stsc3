"klank"
| mk |
mk := [ :freq |
  | t r |
  t := Impulse freq: 0.125 phase: 0 mul: 0.1.
  r := KlankSpec freq: freq amp: nil time: #(16).
  Klank input: t freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: r].
k1 := mk value: #(221 614 1145 1804 2577 3456 4419).
k2 := mk value: #(977 1003 1390 1414 1432 1465 1748 1834 1919 1933 1987 2096 2107 2202 2238 2280 2400 2435 2507 2546 2608 2652 2691 2708).
{k1. k2} * 0.1

"---- KlankSpec"
KlankSpec freq: #(221 614 1145 1804 2577 3456 4419) amp: nil time: #(16)
