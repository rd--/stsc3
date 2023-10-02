(* https://sccode.org/1-5d6 (jpd) ; requires buf=10,buf=11 *)
var bufnum = 10;
var envBufnum = 11;
var pan = 0;
var stretch = 50;
var window = 0.25;
var amp = 1;
var sampleRate = SampleRate();
var fftSize = 2 ^ (window * sampleRate).Log2.Floor;
var trigPeriod = fftSize / sampleRate;
var trig = Impulse(1 / trigPeriod, 0);
var bufdur = BufDur(bufnum);
var pos = DmdFor(trig, 0, Dseries(inf, 0, trigPeriod / (stretch * bufdur)));
var grains = [
	GrainBuf(1, trig, trigPeriod, bufnum, 1, pos, 2, 0, envBufnum, 512),
	GrainBuf(1, trig, trigPeriod, bufnum, 1, pos + (trigPeriod / (2 * stretch * bufdur)), 2, 0, envBufnum, 512)
] * amp;
var diffused = grains.collect { :item |
	var c1 = Fft(BufAlloc(1, fftSize), item, 1, -1, 1, 0);
	var c2 = PvDiffuser(c1, 1 - trig);
	Ifft(c2, -1, 0)
};
var enveloped = diffused * PlayBuf(1, envBufnum, 1 / trigPeriod, 1, 0, 1, 0);
var delayed = DelayC(enveloped.second, trigPeriod / 2, trigPeriod / 2);
Pan2(enveloped.first + delayed, pan, 1)

(* ---- ; allocate buffers ; see ~/sw/hsc3-graphs/lib/scd/graph/jpd-1-5d6.scd *)
