"why supercollider (jmcc) #0"
| s z x |
s := 10 mixFill: [ Resonz in: (Dust density: 0.2 mul: 50) freq: (200.0 rand: 3200.0) bwr: 0.003 ].
z := DelayN in: s maxdelaytime: 0.048 delaytime: 0.048.
x := 7 mixFill: [ CombL in: z maxdelaytime: 0.1 delaytime: (LFNoise1 freq: 0.1 rand mul: 0.04 add: 0.05) decaytime: 15 ].
4 timesRepeat: [ x := AllpassN in: x maxdelaytime: 0.050 delaytime: [ 0.05 rand ] dup decaytime: 1 ].
x * 0.2 + s
