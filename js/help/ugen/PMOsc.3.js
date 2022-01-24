// PMOsc ; event control
var s;
s = sum(Voicer(16, function(e) { var cps, cpsv, mfreq, ix;
cps = midiCps(add(mul(x(e), 24), 42));
cpsv = add(cps, mul(mul(cps, SinOsc(add(mul(y(e), 4), 4), 0)), 0.02));
mfreq = mul(LinLin(LFPulse(fdiv(1, 8), 0, 0.5), 0, 1, 1.01, 2.01), cps);
ix = TXLine(3, 0.001, 0.2, w(e));
return mul(mul(PMOsc(cpsv, mfreq, ix, 0), z(e)), w(e));
}));
XFade2(s, GVerb(BPF(s, midiCps(90), 1), 50, 5, 0.5, 0.5, 15, 0, 0.7, 0.5, 300), 0.2, 1)
