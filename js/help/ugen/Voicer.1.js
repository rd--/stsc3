// Voicer ; error!
mul(sum(Voicer(16, function(e) {
    var freq;
    freq = midiCps(add(mul(eventX(e), [25, add(mul(LFNoise2(0.25), 0.25), 25)]), 48));
    return Pan2(sum(RLPF(Saw(freq), mul(add(eventY(e), 0.75), freq), mul(eventY(e), 0.5))), sub(mul(eventO(e), 2), 1), mul(eventW(e), eventZ(e)));
})), 0.25)
