;; https://gist.github.com/nhthn/8f0dcc1c85e662fe5bf7d0132155c0af (nh)
var index = MouseY(1, 7000, 1, 0.2);
var freq = MouseX(0.01, 40, 1, 0.2);
var divide = 1;
var cutoff = 1;
var snd = LocalIn(4, 0);
snd := Latch(snd, PulseDivider(snd.reversed, Lpf(snd, 10 * cutoff) * 100000 * divide, 0));
snd := SinOsc({ ExpRand(10, 100) } ! 4 * freq + (snd * index), 0);
snd := Latch(snd, PulseDivider(snd.reversed, Lpf(snd, 10 * cutoff).cubed * 8000 * divide, 0));
snd := Lpf(snd, 440);
snd := Select(Lpf(snd.reversed, 10 * cutoff) * 3, [Pulse(440 * freq + (snd * index), 0.5), SinOsc(100 * freq + (snd * index), 0)]);
snd := Rlpf(snd, 440 + (1000 * Clip(Lpf(Lpf(snd, 1), 1), 0, 1)), 0.1);
snd := Sanitize(snd, 0);
4.timesRepeat {
	snd := snd + SinOsc(ExpRand(10, 800) * freq + Lpf(snd * index, 100), 0);
};
snd := snd <! LocalOut(snd);
snd := snd.Splay2;
snd.Clip2(1) * -12.DbAmp

;; https://gist.github.com/nhthn/8f0dcc1c85e662fe5bf7d0132155c0af (nh)
var freqScale = MouseY(0.01, 3, 1, 0.2);
var snd = LocalIn(4, 0);
var freq = 3000 * freqScale + ((Lpf(snd, 0.1) * 100) * [1000, 1500, 1200, 1100]);
snd := Rlpf(Saw(Lpf(freq.abs, 1) * snd), 1000 * freqScale + (900 * snd), snd.abs + 0.01);
snd := Select(PulseDivider(Bpf(snd, 0.1, 0.1), 1000, 0), [
	snd,
	Rlpf(snd, 1000 * freqScale + (100 * Lpf(snd, 0.1).Fold2(1)), 0.5).tanh
]);
snd := LeakDc(Sanitize(snd, 0).Clip2(1), 0.995);
snd := [2, 3, 4, 1].collect { :index | snd[index] };
snd := snd <! LocalOut(snd.Clip2(1));
snd := Splay(snd, 0.5, 1, 0, true);
snd.Clip2(1) * -12.DbAmp
