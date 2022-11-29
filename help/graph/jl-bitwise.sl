;; bitwise (jl) - a0f253ff89f6b244ea29a1e431dd9e5df5571a8b (jonatan liljedahl)
var t = PulseCount(Impulse(8000, 0), 0);
var b1 = (t * 15).bitAnd(t.bitShiftRight(5));
var b2 = (t * 5).bitAnd(t.bitShiftRight([3, 4]));
var b3 = (t * 2).bitAnd(t.bitShiftRight(9));
var b4 = (t * 8).bitAnd(t.bitShiftRight(11));
var s = (b1.bitOr(b2).bitOr(b3).bitOr(b4) - 3) % 256;
Hpf(((s / 127) - 1) * 3, 20).tanh * 0.02

;; bitwise (jl) - a0f253ff89f6b244ea29a1e431dd9e5df5571a8b (jonatan liljedahl) ; abstracted
var t = PulseCount(Impulse(8000, 0), 0);
var f = { :p | (t * p.key).bitAnd(t.bitShiftRight(p.value)) };
var b = [15 -> 5, 5 -> [3, 4], 2 -> 9, 8 -> 11].collect(f);
var s = (b.injectInto(0, { :j :i | i.bitOr(j) }) - 3) % 256;
Hpf(((s / 127) - 1) * 3, 20).tanh * 0.02