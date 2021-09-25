|n x y|
 n := 28 .
 x := [ :i| |a b c d e f g h j k z l|
 a := (LFSaw apply: {i + 1 * 5 . 0}) * 0.5 * (Float pi) .
 b := (SinOsc apply: {i + 1 * 0.001 . 0}) * 0.5 .
 c := (LFSaw apply: {0.2 + b . i / n}) * 0.4 .
 d := (SinOsc apply: {0.03 . i + 1}) * 0.5 + 1 .
 e := (SinOsc apply: {200 + i . 0}) * d .
 f := (SinOsc apply: {0.04 . i + 2}) * 0.5 + 1 .
 g := (SinOsc apply: {400 + i . 0}) * f .
 h := (SinOsc apply: {0.05 . i + 3}) * 0.5 + 1 .
 j := (SinOsc apply: {800 + i . 0}) * h .
 k := (LinExp apply: {i . 0 . n - 1 . 70 . 1500}) .
 z := (SinOsc apply: {k . a}) * (c max: 0) * e * g * j .
 l := (LinLin apply: {i . 0 . n - 1 . -0.925 . 0.925}) .
 (Pan2 apply: {z . l . 1 / n}) .
 ] .
 y := (Limiter apply: {(LeakDC apply: {n mixFill: x . 0.995}) . 1 . 0.01}) .
 (GVerb apply: {y . 3 . 5 . 0.2 . 0.8 . 20 . 0.1 . 0.7 . 0.5 . 300}) * 0.25 .
