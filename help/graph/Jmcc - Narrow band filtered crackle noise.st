"narrow band filtered crackle noise (jmcc) #2"
| rf1 rf2 rf c |
rf1 := (0.0 rand: 2000.0) + 80 .
rf2 := rf1 + ((-0.2 rand: 0.2) * rf1) .
rf := XLine start: rf1 end: rf2 dur: 9 doneAction: 0 .
c := Crackle chaosParam: (1.97 + (0.0 rand: 0.03)) .
Pan2 in: (Resonz in: c freq: rf kr bwr: 0.2) pos: (-1.0 rand: 1.0) level: 0.15
