"20070423 ; rd ; requires=kr"
| eggcrate p x y t f0 f1 f a |
eggcrate := [ :u :v| (u * Float pi) cos * (v * Float pi) sin ].
p := #(64 72 96 128 256 6400 7200 8400 9600).
x := BrownNoise new.
y := BrownNoise new.
t := Dust density: 2.4.
f0 := TChoose trig: t array: p.
f1 := TChoose trig: t array: p.
f := LinLin in: (eggcrate value: x value: y) srclo: -1 srchi: 1 dstlo: f0 dsthi: f1.
a := LinLin in: x srclo: -1 srchi: 1 dstlo: 0 dsthi: 0.1.
Pan2 in: (SinOsc freq: f kr phase: 0) pos: y kr level: a kr
