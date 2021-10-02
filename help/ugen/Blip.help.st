"blip ; event control"
| f |
f := [ :e |
  | o |
  o := (Blip freq: (e x * 13 + 48) midicps numharm: e y * 19 + 1).
  Pan2 in: o pos: e o * 2 - 1 level: e z * e w * 0.5 ].
(EventControl numVoices: 16 voiceFunc: f) mix

"Blip ; resonz"
| p f |
p := #((  4 1  50   30 120 120  0.4)
       (  8 1  50  130  10 120  0.2)
       (  4 2 120  300   1 120  0.5)
       (182 2 120  430  10  80  0.5)
       (442 1  50  130  10 120  0.2)
       ( 82 1  50   10   1 120  0.2)
       (421 2 120   30   1 120  0.2)
       ( 28 2 120  430  10 120  0.2)).
f := [:x |
  | f1 f2 t1 f3 f4 t2 q o |
  f1 := x at: 1. f2 := x at: 2. t1 := x at: 3. f3 := x at: 4. f4 := x at: 5. t2 := x at: 6. q := x at: 7.
  o := Blip freq: (Line start: f1 end: f2 dur: t1 doneAction: 0) numharm: 200 mul: 0.5.
  Resonz in: o freq: (Line start: f3 end: f4 dur: t2 doneAction: 0) bwr: q].
(p collect: f) sum
