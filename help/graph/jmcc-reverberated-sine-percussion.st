"reverberated sine percussion (jmcc) #3"
[:d :c :a |
 |s z y_ y x |
 s := d mixFill: [:_i| Resonz in: (Dust density: (2 / d) mul: 50) freq: (Rand lo: 200 hi: 3200) bwr: 0.003] .
 z := DelayN in: s maxdelaytime: 0.048 delaytime: 0.048 .
 y_ := LFNoise1 freq: (c mceFill: [:_i| Rand lo: 0 hi: 0.1]) mul: 0.04 add: 0.05 .
 y := (CombL in: z maxdelaytime: 0.1 delaytime: (y_ kr) decaytime: 15) mix .
 x := y .
 (1 to: a) do: [:_i| x := AllpassN in: x maxdelaytime: 0.050 delaytime: {Rand lo: 0 hi: 0.05 . Rand lo: 0 hi: 0.05} decaytime: 1] .
 x mul: 0.2 add: s] value: 6 value: 5 value: 4
