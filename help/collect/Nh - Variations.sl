(* https://gist.github.com/nhthn/8f0dcc1c85e662fe5bf7d0132155c0af (nh) *)
var freqScale = MouseY(0.01, 3, 1, 0.2);
var snd = LocalIn(4, 0);
var freq = 3000 * freqScale + ((Lpf(snd, 0.1) * 100) * [1000, 1500, 1200, 1100]);
snd := Rlpf(Saw(Lpf(freq.Abs, 1) * snd), 1000 * freqScale + (900 * snd), snd.Abs + 0.01);
snd := Select(PulseDivider(Bpf(snd, 0.1, 0.1), 1000, 0), [
	snd,
	Rlpf(snd, 1000 * freqScale + (100 * Lpf(snd, 0.1).Fold2(1)), 0.5).Tanh
]);
snd := LeakDc(Sanitize(snd, 0).Clip2(1), 0.995);
snd := [2, 3, 4, 1].collect { :index | snd[index] };
snd := snd <! LocalOut(snd.Clip2(1));
snd := Splay2(snd, 0.5, 1, 0, true);
snd.Clip2(1) * -12.DbAmp
