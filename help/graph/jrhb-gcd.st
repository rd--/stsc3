"generative landscape (jrhb) ; https://www.listarc.bham.ac.uk/lists/sc-users/msg68991.html"
| x y i d a z1 z2 freq c |
x := MouseX minval: -10 maxval: 10 warp: 0 lag: 0.2.
y := MouseY minval: 0 maxval: 5 warp: 0 lag: 0.2.
i := LFSaw freq: 0.062 iphase: 0 mul: 10 add: (y * (0 to: 8)).
d := i frac.
a := (ModDif x: d y: 0.5 mod: 1) negated + 0.8.
z1 := (i gcd: x) + d.
z2 := (i lcm: x) + d.
freq := {z1. z2} transpose abs + 1 * 120.
c := AmpComp freq: freq kr root: 261 exp: 0.33 mul: a.
(SinOsc freq: freq kr phase: 0 mul: c kr) sum * 0.01
