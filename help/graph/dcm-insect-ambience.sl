;; https://scsynth.org/t/insect-ambience/94/ (dcm) ; backgrdhiss ; requires=kr
{ HPF(Crackle(1.96), 900) * Vibrato(50.0, 1, 0.02, 0, 0, 0.04, 0.1, 0, 0).kr * 0.0001 }.dup

;; bugs
var amp = 1.2, ampLagTime = 20, cut_freq = 600, nse = 0.1, n_freq = 800;
var src = { LPF(PinkNoise() * nse, n_freq) * SinOsc(1750, 0) * SinOsc(160, 0) }.dup;
var bugs = PitchShift(src, 0.2, LinLin(LFTri(20, 0), -1, 1, 0.9, 5), 0.01, 0.0001);
HPF(Limiter(bugs, 0.9, 0.01) * Ln(0, amp, ampLagTime), cut_freq)

;; frog
Blip([10.001,10], 200) * LFPulse(0.1, 0, 0.5) * 0.02

;; thestart (modified)
var amp = 0.2, ampLagTime = 5, freq = 1000, pos = 1, posLagTime = 30;
var gen = { HPF(BPF(PinkNoise() * Ln(0, amp, ampLagTime), [450, 250, 150], 0.2), LinLin(LFNoise2([0.1, 0.11, 0.14]), -1, 1, 100,800)) };
var src = gen.dup(3).sum.Splay2;
XFade2(src, LPF(src, freq), Ln(0, pos, posLagTime), 1)

;; StaticLoss ; requries=WaveLoss
var amp = 1, freq = 1200, rq = 1, db = -45, gate = 1, pan = -0.5;
var src = WaveLoss(PinkNoise() * 0.1, 39, 40, 1);
Pan2(BPeakEQ(src, freq, rq, db), pan, amp)

;; Storm ; very slow fade-in
var amp = 0.2, ampLagTime = 102, cut = 165, hicut = 440;
var src = { RLPF(Crackle(1.9 + 0.05.rand), cut, 1) }.dup(6).sum;
src = HPF(LeakDC(Decay2(src, 0.01, 0.1), 0.995), hicut);
Pan2(Limiter(src, 0.9, 0.1), 0, Ln(0, amp, ampLagTime))

;; Rumble ; very slow fade-in
var amp = 0.2, lagTime = 120, freq = 240, frq = 490;
var src = LeakDC(Decay2(ClipNoise() * 0.2, 0.01, 0.1), 0.995);
Pan2(LeakDC(LPF(RLPF(src, freq, 1), frq), 0.995), 0, Ln(0, amp, lagTime))
