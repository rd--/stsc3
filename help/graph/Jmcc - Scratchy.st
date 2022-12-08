"scratchy (jmcc) #1"
| n f |
n := ([ BrownNoise new ] dup: 2) * 0.5.
f := ((n - 0.49) max: 0) * 20.
Rhpf in: f freq: 5000 rq: 1
