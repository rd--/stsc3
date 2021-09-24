"scratchy (jmcc) #1"
| n f |
n := (2 mceFill: [ :x | BrownNoise new ]) * 0.5.
f := ((n - 0.49) max: 0) * 20.
RHPF in: f freq: 5000 rq: 1
