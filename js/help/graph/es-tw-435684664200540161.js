// tw-435684664200540161 (es)
var vc;
vc = function(i) {
    var m;
    m = add(mod(i, add(mul(LFNoise2(0.01), 50), 50)), add(mul(LFNoise2(0.1), 10), 40));
    return mul(SinOsc(midiCps(m), 0), add(mul(LFNoise2(1), 0.01), 0.01));
};
Splay2(collect(to(1, 99), vc))
