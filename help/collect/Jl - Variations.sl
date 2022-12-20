;; bitwise (jl) - a0f253ff89f6b244ea29a1e431dd9e5df5571a8b (jonatan liljedahl) ; abstracted
var t = PulseCount(Impulse(8000, 0), 0);
var f = { :p | (t * p.key).BitAnd(t.ShiftRight(p.value)) };
var b = [15 -> 5, 5 -> [3, 4], 2 -> 9, 8 -> 11].collect(f);
var s = (b.injectInto(0, { :j :i | i.BitOr(j) }) - 3) % 256;
Hpf(((s / 127) - 1) * 3, 20).Tanh * 0.02
