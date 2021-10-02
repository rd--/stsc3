"reverberated sine percussion (jmcc) #3"
| d s z t y x |
d := 6.
s := d mixFill: [ Resonz in: (Dust density: 2 / d mul: 50) freq: (Rand lo: 200 hi: 3200) bwr: 0.003 ].
z := DelayN in: s maxdelaytime: 0.048 delaytime: 0.048.
t := LFNoise1 freq: ([Rand lo: 0 hi: 0.1] dup: 5) mul: 0.04 add: 0.05.
y := (CombL in: z maxdelaytime: 0.1 delaytime: t kr decaytime: 15) mix.
x := y.
4 timesRepeat: [ x := AllpassN in: x maxdelaytime: 0.050 delaytime: [Rand lo: 0 hi: 0.05] dup decaytime: 1 ].
x * 0.2 + s
