"theremin (jmcc)"
|mod detune x y f a o|
mod := 7 .
detune := 0 .
x := (MouseX minval: 0 maxval: 0.9 warp: 0 lag: 0.2) kr .
y := ((MouseY minval: 4000 maxval: 200 warp: 1 lag: 0.8) + detune) kr .
f := y + ((y * (SinOsc freq: mod phase: 0)) * 0.02) .
a := (SinOsc freq: f phase: 0) * x .
o := Pan2 in: a pos: 0 level: 0.1 .
o
