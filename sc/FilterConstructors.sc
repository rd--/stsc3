+ AllpassC { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassC.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ AllpassL { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassL.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ AllpassN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassN.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ BBandPass { *new { arg in = 0.0, freq = 1200.0, bw = 1.0;^BBandPass.multiNew(in.rate, in, freq, bw) } }
+ BBandStop { *new { arg in = 0.0, freq = 1200.0, bw = 1.0;^BBandStop.multiNew(in.rate, in, freq, bw) } }
+ BLowPass { *new { arg in = 0.0, freq = 1200.0, rq = 1.0;^BLowPass.multiNew(in.rate, in, freq, rq) } }
+ BPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^BPF.multiNew(in.rate, in, freq, rq) } }
+ BPZ2 { *new { arg in = 0.0;^BPZ2.multiNew(in.rate, in) } }
+ BRF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^BRF.multiNew(in.rate, in, freq, rq) } }
+ BufWr { *new { arg bufnum = 0.0, phase = 0.0, loop = 1.0, inputArray = 0.0;"sc_filter_constructor: reordering not implemented: [3,0,1,2] ".error; ^BufWr.multiNew(inputArray.rate, bufnum, phase, loop, inputArray) } }
+ Clip { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Clip.multiNew(in.rate, in, lo, hi) } }
+ CombC { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombC.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ CombL { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombL.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ CombN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombN.multiNew(in.rate, in, maxdelaytime, delaytime, decaytime) } }
+ Decay { *new { arg in = 0.0, decayTime = 1.0;^Decay.multiNew(in.rate, in, decayTime) } }
+ Decay2 { *new { arg in = 0.0, attackTime = 1.0e-2, decayTime = 1.0;^Decay2.multiNew(in.rate, in, attackTime, decayTime) } }
+ DegreeToKey { *new { arg bufnum = 0.0, in = 0.0, octave = 12.0;^DegreeToKey.multiNew(in.rate, bufnum, in, octave) } }
+ DelayN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2;^DelayN.multiNew(in.rate, in, maxdelaytime, delaytime) } }
+ Demand { *new { arg trig = 0.0, reset = 0.0, demandUGens = 0.0;^Demand.multiNew(trig.rate, trig, reset, demandUGens) } }
+ DetectSilence { *new { arg in = 0.0, amp = 1.0e-4, time = 0.1, doneAction = 0.0;^DetectSilence.multiNew(in.rate, in, amp, time, doneAction) } }
+ Fold { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Fold.multiNew(in.rate, in, lo, hi) } }
+ FreeVerb { *new { arg in = 0.0, mix = 0.33, room = 0.5, damp = 0.5;^FreeVerb.multiNew(in.rate, in, mix, room, damp) } }
+ FreeVerb2 { *new { arg in = 0.0, in2 = 0.0, mix = 0.33, room = 0.5, damp = 0.5;^FreeVerb2.multiNew(in.rate, in, in2, mix, room, damp) } }
+ GVerb { *new { arg in = 0.0, roomsize = 10.0, revtime = 3.0, damping = 0.5, inputbw = 0.5, spread = 15.0, drylevel = 1.0, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300.0;^GVerb.multiNew(in.rate, in, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize) } }
+ Hasher { *new { arg in = 0.0;^Hasher.multiNew(in.rate, in) } }
+ HPF { *new { arg in = 0.0, freq = 440.0;^HPF.multiNew(in.rate, in, freq) } }
+ HPZ1 { *new { arg in = 0.0;^HPZ1.multiNew(in.rate, in) } }
+ InRange { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^InRange.multiNew(in.rate, in, lo, hi) } }
+ Klank { *new { arg input = 0.0, freqscale = 1.0, freqoffset = 0.0, decayscale = 1.0, specificationsArrayRef = 0.0;"sc_filter_constructor: reordering not implemented: [4,0,1,2,3] ".error; ^Klank.multiNew(input.rate, input, freqscale, freqoffset, decayscale, specificationsArrayRef) } }
+ LPF { *new { arg in = 0.0, freq = 440.0;^LPF.multiNew(in.rate, in, freq) } }
+ Lag { *new { arg in = 0.0, lagTime = 0.1;^Lag.multiNew(in.rate, in, lagTime) } }
+ LagUD { *new { arg in = 0.0, lagTimeU = 0.1, lagTimeD = 0.1;^LagUD.multiNew(in.rate, in, lagTimeU, lagTimeD) } }
+ Lag2 { *new { arg in = 0.0, lagTime = 0.1;^Lag2.multiNew(in.rate, in, lagTime) } }
+ Lag3 { *new { arg in = 0.0, lagTime = 0.1;^Lag3.multiNew(in.rate, in, lagTime) } }
+ Lag3UD { *new { arg in = 0.0, lagTimeU = 0.1, lagTimeD = 0.1;^Lag3UD.multiNew(in.rate, in, lagTimeU, lagTimeD) } }
+ LeakDC { *new { arg in = 0.0, coef = 0.995;^LeakDC.multiNew(in.rate, in, coef) } }
+ Limiter { *new { arg in = 0.0, level = 1.0, dur = 1.0e-2;^Limiter.multiNew(in.rate, in, level, dur) } }
+ LinExp { *new { arg in = 0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;^LinExp.multiNew(in.rate, in, srclo, srchi, dstlo, dsthi) } }
+ LinPan2 { *new { arg in = 0.0, pos = 0.0, level = 1.0;^LinPan2.multiNew(in.rate, in, pos, level) } }
+ LocalOut { *new { arg channelsArray = 0.0;^LocalOut.multiNew(channelsArray.rate, channelsArray) } }
+ MantissaMask { *new { arg in = 0.0, bits = 3.0;^MantissaMask.multiNew(in.rate, in, bits) } }
+ ModDif { *new { arg x = 0.0, y = 0.0, mod = 1.0;^ModDif.multiNew(x.rate, x, y, mod) } }
+ MoogFF { *new { arg in = 0.0, freq = 100.0, gain = 2.0, reset = 0.0;^MoogFF.multiNew(in.rate, in, freq, gain, reset) } }
+ MoogLadder { *new { arg in = 0.0, ffreq = 440.0, res = 0.0;^MoogLadder.multiNew(in.rate, in, ffreq, res) } }
+ Normalizer { *new { arg in = 0.0, level = 1.0, dur = 1.0e-2;^Normalizer.multiNew(in.rate, in, level, dur) } }
+ OnePole { *new { arg in = 0.0, coef = 0.5;^OnePole.multiNew(in.rate, in, coef) } }
+ Out { *new { arg bus = 0.0, channelsArray = 0.0;^Out.multiNew(channelsArray.rate, bus, channelsArray) } }
+ Pan2 { *new { arg in = 0.0, pos = 0.0, level = 1.0;^Pan2.multiNew(in.rate, in, pos, level) } }
+ PitchShift { *new { arg in = 0.0, windowSize = 0.2, pitchRatio = 1.0, pitchDispersion = 0.0, timeDispersion = 0.0;^PitchShift.multiNew(in.rate, in, windowSize, pitchRatio, pitchDispersion, timeDispersion) } }
+ Pluck { *new { arg in = 0.0, trig = 1.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0, coef = 0.5;^Pluck.multiNew(in.rate, in, trig, maxdelaytime, delaytime, decaytime, coef) } }
+ PulseCount { *new { arg trig = 0.0, reset = 0.0;^PulseCount.multiNew(trig.rate, trig, reset) } }
+ PulseDivider { *new { arg trig = 0.0, div = 2.0, start = 0.0;^PulseDivider.multiNew(trig.rate, trig, div, start) } }
+ RHPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^RHPF.multiNew(in.rate, in, freq, rq) } }
+ RLPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^RLPF.multiNew(in.rate, in, freq, rq) } }
+ Resonz { *new { arg in = 0.0, freq = 440.0, bwr = 1.0;^Resonz.multiNew(in.rate, in, freq, bwr) } }
+ Ringz { *new { arg in = 0.0, freq = 440.0, decaytime = 1.0;^Ringz.multiNew(in.rate, in, freq, decaytime) } }
+ RunningMax { *new { arg in = 0.0, trig = 0.0;^RunningMax.multiNew(in.rate, in, trig) } }
+ RTScramble { *new { arg trigger = 0.0, inputs = 0.0;^RTScramble.multiNew(trigger.rate, trigger, inputs) } }
+ Slope { *new { arg in = 0.0;^Slope.multiNew(in.rate, in) } }
+ Stepper { *new { arg trig = 0.0, reset = 0.0, min = 0.0, max = 7.0, step = 1.0, resetval = 0.0;^Stepper.multiNew(trig.rate, trig, reset, min, max, step, resetval) } }
+ Sweep { *new { arg trig = 0.0, rate = 1.0;^Sweep.multiNew(trig.rate, trig, rate) } }
+ TExpRand { *new { arg lo = 1.0e-2, hi = 1.0, trig = 0.0;^TExpRand.multiNew(trig.rate, lo, hi, trig) } }
+ Timer { *new { arg trig = 0.0;^Timer.multiNew(trig.rate, trig) } }
+ TIRand { *new { arg lo = 0.0, hi = 127.0, trig = 0.0;^TIRand.multiNew(trig.rate, lo, hi, trig) } }
+ ToggleFF { *new { arg trig = 0.0;^ToggleFF.multiNew(trig.rate, trig) } }
+ TRand { *new { arg lo = 0.0, hi = 1.0, trig = 0.0;^TRand.multiNew(trig.rate, lo, hi, trig) } }
+ Trig { *new { arg in = 0.0, dur = 0.1;^Trig.multiNew(in.rate, in, dur) } }
+ Trig1 { *new { arg in = 0.0, dur = 0.1;^Trig1.multiNew(in.rate, in, dur) } }
+ Wrap { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Wrap.multiNew(in.rate, in, lo, hi) } }
