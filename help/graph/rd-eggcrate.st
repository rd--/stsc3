"eggcrate ; semiautotranslate"
|eggcrate p x y t f0 f1 f a|
eggcrate := [:u :v| (u * Float pi) cos * (v * Float pi) sin].
p := #(64 72 96 128 256 6400 7200 8400 9600).
x := BrownNoise kr.
y := BrownNoise kr.
t := Dust kr: #(2.4).
f0 := TChoose kr: {t. p}.
f1 := TChoose kr: {t. p}.
f := (eggcrate value: x value: y) inRangeFrom: f0 to: f1.
a := x inRangeFrom: 0 to: 0.1.
Pan2 ar: {(SinOsc ar: {f kr. 0}) mix. y. a kr}.
