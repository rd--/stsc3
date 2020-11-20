"narrow band filtered crackle noise (jmcc) #2 ; texture=spawn,2,inf"
|e rf1 rf2 rf c|
e := EnvLinen attackTime: 2 sustainTime: 5 releaseTime: 2 level: 1 curves: #(lin lin lin) .
rf1 := (0.0 rand: 2000.0) + 80 .
rf2 := rf1 + ((-0.2 rand: 0.2) * rf1) .
rf := XLine start: rf1 end: rf2 dur: 9 doneAction: 0 .
c := Crackle chaosParam: (1.97 + (0.0 rand: 0.03)) mul: 0.15 .
Pan2 in: (Resonz in: c freq: rf kr bwr: 0.2) pos: (-1.0 rand: 1.0) level: (e envGen: 1 doneAction: 2) kr
