;; https://twitter.com/headcube/status/474064500564324352 (nv)
var k = 5; (* 9 => 5 udp *)
var o = { :ix | { LfPulse(2 ** IRand(-9, 1), IRand(0, 2) / 2, 0.5) }.dup(ix + 1).product / ix + 1 };
var z = { var f = (1 .. 8).collect(o).product * 86; Pluck(Bpf(f, f, 1).Sin, Saw(440), 1 , 1 / f, 9, 0.5) };
Splay2(z ! k) * 0.2
