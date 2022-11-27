;; ModDif ; wrapping amplitude crossfade
var nc = 12;
var x = SinOsc(RandN(nc, 300, 800), 0);
var d = ModDif (MouseX(0, nc * 2, 0, 0.2), [0 .. nc - 1], nc);
Splay2(x * (1 - d).max(0)) * 0.1
