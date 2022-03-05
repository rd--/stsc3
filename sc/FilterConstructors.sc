+ AllpassC { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassC.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ AllpassL { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassL.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ AllpassN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^AllpassN.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ Balance2 { *new { arg left = 0.0, right = 0.0, pos = 0.0, level = 1.0;^Balance2.performList(left.rate.rateToSelector, [left, right, pos, level]) } }
+ BBandPass { *new { arg in = 0.0, freq = 1200.0, bw = 1.0;^BBandPass.performList(in.rate.rateToSelector, [in, freq, bw]) } }
+ BBandStop { *new { arg in = 0.0, freq = 1200.0, bw = 1.0;^BBandStop.performList(in.rate.rateToSelector, [in, freq, bw]) } }
+ BLowPass { *new { arg in = 0.0, freq = 1200.0, rq = 1.0;^BLowPass.performList(in.rate.rateToSelector, [in, freq, rq]) } }
+ BPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^BPF.performList(in.rate.rateToSelector, [in, freq, rq]) } }
+ BPZ2 { *new { arg in = 0.0;^BPZ2.performList(in.rate.rateToSelector, [in]) } }
+ BRF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^BRF.performList(in.rate.rateToSelector, [in, freq, rq]) } }
+ BRZ2 { *new { arg in = 0.0;^BRZ2.performList(in.rate.rateToSelector, [in]) } }
+ BufWr { *new { arg inputArray = 0.0, bufnum = 0.0, phase = 0.0, loop = 1.0;^BufWr.performList(inputArray.rate.rateToSelector, [inputArray, bufnum, phase, loop]) } }
+ Clip { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Clip.performList(in.rate.rateToSelector, [in, lo, hi]) } }
+ CoinGate { *new { arg prob = 0.0, in = 0.0;^CoinGate.performList(in.rate.rateToSelector, [prob, in]) } }
+ CombC { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombC.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ CombL { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombL.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ CombN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;^CombN.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime, decaytime]) } }
+ Decay { *new { arg in = 0.0, decayTime = 1.0;^Decay.performList(in.rate.rateToSelector, [in, decayTime]) } }
+ Decay2 { *new { arg in = 0.0, attackTime = 1.0e-2, decayTime = 1.0;^Decay2.performList(in.rate.rateToSelector, [in, attackTime, decayTime]) } }
+ DegreeToKey { *new { arg bufnum = 0.0, in = 0.0, octave = 12.0;^DegreeToKey.performList(in.rate.rateToSelector, [bufnum, in, octave]) } }
+ Delay1 { *new { arg in = 0.0;^Delay1.performList(in.rate.rateToSelector, [in]) } }
+ Delay2 { *new { arg in = 0.0;^Delay2.performList(in.rate.rateToSelector, [in]) } }
+ DelayC { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2;^DelayC.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime]) } }
+ DelayL { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2;^DelayL.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime]) } }
+ DelayN { *new { arg in = 0.0, maxdelaytime = 0.2, delaytime = 0.2;^DelayN.performList(in.rate.rateToSelector, [in, maxdelaytime, delaytime]) } }
+ Demand { *new { arg trig = 0.0, reset = 0.0, demandUGens = 0.0;^Demand.performList(trig.rate.rateToSelector, [trig, reset, demandUGens]) } }
+ DetectSilence { *new { arg in = 0.0, amp = 1.0e-4, time = 0.1, doneAction = 0.0;^DetectSilence.performList(in.rate.rateToSelector, [in, amp, time, doneAction]) } }
+ Fold { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Fold.performList(in.rate.rateToSelector, [in, lo, hi]) } }
+ Formlet { *new { arg in = 0.0, freq = 440.0, attacktime = 1.0, decaytime = 1.0;^Formlet.performList(in.rate.rateToSelector, [in, freq, attacktime, decaytime]) } }
+ FOS { *new { arg in = 0.0, a0 = 0.0, a1 = 0.0, b1 = 0.0;^FOS.performList(in.rate.rateToSelector, [in, a0, a1, b1]) } }
+ FreeVerb { *new { arg in = 0.0, mix = 0.33, room = 0.5, damp = 0.5;^FreeVerb.performList(in.rate.rateToSelector, [in, mix, room, damp]) } }
+ FreeVerb2 { *new { arg in = 0.0, in2 = 0.0, mix = 0.33, room = 0.5, damp = 0.5;^FreeVerb2.performList(in.rate.rateToSelector, [in, in2, mix, room, damp]) } }
+ Gate { *new { arg in = 0.0, trig = 0.0;^Gate.performList(in.rate.rateToSelector, [in, trig]) } }
+ GVerb { *new { arg in = 0.0, roomsize = 10.0, revtime = 3.0, damping = 0.5, inputbw = 0.5, spread = 15.0, drylevel = 1.0, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300.0;^GVerb.performList(in.rate.rateToSelector, [in, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize]) } }
+ Hasher { *new { arg in = 0.0;^Hasher.performList(in.rate.rateToSelector, [in]) } }
+ HPF { *new { arg in = 0.0, freq = 440.0;^HPF.performList(in.rate.rateToSelector, [in, freq]) } }
+ HPZ1 { *new { arg in = 0.0;^HPZ1.performList(in.rate.rateToSelector, [in]) } }
+ HPZ2 { *new { arg in = 0.0;^HPZ2.performList(in.rate.rateToSelector, [in]) } }
+ Index { *new { arg bufnum = 0.0, in = 0.0;^Index.performList(in.rate.rateToSelector, [bufnum, in]) } }
+ IndexInBetween { *new { arg bufnum = 0.0, in = 0.0;^IndexInBetween.performList(in.rate.rateToSelector, [bufnum, in]) } }
+ InRange { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^InRange.performList(in.rate.rateToSelector, [in, lo, hi]) } }
+ Integrator { *new { arg in = 0.0, coef = 1.0;^Integrator.performList(in.rate.rateToSelector, [in, coef]) } }
+ Klank { *new { arg specificationsArrayRef = 0.0, input = 0.0, freqscale = 1.0, freqoffset = 0.0, decayscale = 1.0;^Klank.performList(input.rate.rateToSelector, [specificationsArrayRef, input, freqscale, freqoffset, decayscale]) } }
+ LPF { *new { arg in = 0.0, freq = 440.0;^LPF.performList(in.rate.rateToSelector, [in, freq]) } }
+ Lag { *new { arg in = 0.0, lagTime = 0.1;^Lag.performList(in.rate.rateToSelector, [in, lagTime]) } }
+ LagUD { *new { arg in = 0.0, lagTimeU = 0.1, lagTimeD = 0.1;^LagUD.performList(in.rate.rateToSelector, [in, lagTimeU, lagTimeD]) } }
+ Lag2 { *new { arg in = 0.0, lagTime = 0.1;^Lag2.performList(in.rate.rateToSelector, [in, lagTime]) } }
+ Lag3 { *new { arg in = 0.0, lagTime = 0.1;^Lag3.performList(in.rate.rateToSelector, [in, lagTime]) } }
+ Lag3UD { *new { arg in = 0.0, lagTimeU = 0.1, lagTimeD = 0.1;^Lag3UD.performList(in.rate.rateToSelector, [in, lagTimeU, lagTimeD]) } }
+ Latch { *new { arg in = 0.0, trig = 0.0;^Latch.performList(in.rate.rateToSelector, [in, trig]) } }
+ LeakDC { *new { arg in = 0.0, coef = 0.995;^LeakDC.performList(in.rate.rateToSelector, [in, coef]) } }
+ Limiter { *new { arg in = 0.0, level = 1.0, dur = 1.0e-2;^Limiter.performList(in.rate.rateToSelector, [in, level, dur]) } }
+ LinExp { *new { arg in = 0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;^LinExp.performList(in.rate.rateToSelector, [in, srclo, srchi, dstlo, dsthi]) } }
+ LinPan2 { *new { arg in = 0.0, pos = 0.0, level = 1.0;^LinPan2.performList(in.rate.rateToSelector, [in, pos, level]) } }
+ LinXFade2 { *new { arg inA = 0.0, inB = 0.0, pan = 0.0;^LinXFade2.performList(inA.rate.rateToSelector, [inA, inB, pan]) } }
+ LocalOut { *new { arg channelsArray = 0.0;^LocalOut.performList(channelsArray.rate.rateToSelector, [channelsArray]) } }
+ LPZ1 { *new { arg in = 0.0;^LPZ1.performList(in.rate.rateToSelector, [in]) } }
+ LPZ2 { *new { arg in = 0.0;^LPZ2.performList(in.rate.rateToSelector, [in]) } }
+ MantissaMask { *new { arg in = 0.0, bits = 3.0;^MantissaMask.performList(in.rate.rateToSelector, [in, bits]) } }
+ Median { *new { arg length = 3.0, in = 0.0;^Median.performList(in.rate.rateToSelector, [length, in]) } }
+ ModDif { *new { arg x = 0.0, y = 0.0, mod = 1.0;^ModDif.performList(x.rate.rateToSelector, [x, y, mod]) } }
+ MoogFF { *new { arg in = 0.0, freq = 100.0, gain = 2.0, reset = 0.0;^MoogFF.performList(in.rate.rateToSelector, [in, freq, gain, reset]) } }
+ Normalizer { *new { arg in = 0.0, level = 1.0, dur = 1.0e-2;^Normalizer.performList(in.rate.rateToSelector, [in, level, dur]) } }
+ OnePole { *new { arg in = 0.0, coef = 0.5;^OnePole.performList(in.rate.rateToSelector, [in, coef]) } }
+ OneZero { *new { arg in = 0.0, coef = 0.5;^OneZero.performList(in.rate.rateToSelector, [in, coef]) } }
+ Out { *new { arg bus = 0.0, channelsArray = 0.0;^Out.performList(channelsArray.rate.rateToSelector, [bus, channelsArray]) } }
+ Pan2 { *new { arg in = 0.0, pos = 0.0, level = 1.0;^Pan2.performList(in.rate.rateToSelector, [in, pos, level]) } }
+ PanAz { *new { arg in = 0.0, pos = 0.0, level = 1.0, width = 2.0, orientation = 0.5;^PanAz.performList(in.rate.rateToSelector, [in, pos, level, width, orientation]) } }
+ PeakFollower { *new { arg in = 0.0, decay = 0.999;^PeakFollower.performList(in.rate.rateToSelector, [in, decay]) } }
+ PitchShift { *new { arg in = 0.0, windowSize = 0.2, pitchRatio = 1.0, pitchDispersion = 0.0, timeDispersion = 0.0;^PitchShift.performList(in.rate.rateToSelector, [in, windowSize, pitchRatio, pitchDispersion, timeDispersion]) } }
+ Pluck { *new { arg in = 0.0, trig = 1.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0, coef = 0.5;^Pluck.performList(in.rate.rateToSelector, [in, trig, maxdelaytime, delaytime, decaytime, coef]) } }
+ PulseCount { *new { arg trig = 0.0, reset = 0.0;^PulseCount.performList(trig.rate.rateToSelector, [trig, reset]) } }
+ PulseDivider { *new { arg trig = 0.0, div = 2.0, start = 0.0;^PulseDivider.performList(trig.rate.rateToSelector, [trig, div, start]) } }
+ RHPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^RHPF.performList(in.rate.rateToSelector, [in, freq, rq]) } }
+ RLPF { *new { arg in = 0.0, freq = 440.0, rq = 1.0;^RLPF.performList(in.rate.rateToSelector, [in, freq, rq]) } }
+ ReplaceOut { *new { arg bus = 0.0, channelsArray = 0.0;^ReplaceOut.performList(channelsArray.rate.rateToSelector, [bus, channelsArray]) } }
+ Resonz { *new { arg in = 0.0, freq = 440.0, bwr = 1.0;^Resonz.performList(in.rate.rateToSelector, [in, freq, bwr]) } }
+ Ringz { *new { arg in = 0.0, freq = 440.0, decaytime = 1.0;^Ringz.performList(in.rate.rateToSelector, [in, freq, decaytime]) } }
+ RunningMax { *new { arg in = 0.0, trig = 0.0;^RunningMax.performList(in.rate.rateToSelector, [in, trig]) } }
+ RunningSum { *new { arg in = 0.0, numsamp = 40.0;^RunningSum.performList(in.rate.rateToSelector, [in, numsamp]) } }
+ Rotate2 { *new { arg x = 0.0, y = 0.0, pos = 0.0;^Rotate2.performList(x.rate.rateToSelector, [x, y, pos]) } }
+ Schmidt { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Schmidt.performList(in.rate.rateToSelector, [in, lo, hi]) } }
+ SetResetFF { *new { arg trig = 0.0, reset = 0.0;^SetResetFF.performList(trig.rate.rateToSelector, [trig, reset]) } }
+ Slew { *new { arg in = 0.0, up = 1.0, dn = 1.0;^Slew.performList(in.rate.rateToSelector, [in, up, dn]) } }
+ Slope { *new { arg in = 0.0;^Slope.performList(in.rate.rateToSelector, [in]) } }
+ SOS { *new { arg in = 0.0, a0 = 0.0, a1 = 0.0, a2 = 0.0, b1 = 0.0, b2 = 0.0;^SOS.performList(in.rate.rateToSelector, [in, a0, a1, a2, b1, b2]) } }
+ Stepper { *new { arg trig = 0.0, reset = 0.0, min = 0.0, max = 7.0, step = 1.0, resetval = 0.0;^Stepper.performList(trig.rate.rateToSelector, [trig, reset, min, max, step, resetval]) } }
+ Sweep { *new { arg trig = 0.0, rate = 1.0;^Sweep.performList(trig.rate.rateToSelector, [trig, rate]) } }
+ TExpRand { *new { arg lo = 1.0e-2, hi = 1.0, trig = 0.0;^TExpRand.performList(trig.rate.rateToSelector, [lo, hi, trig]) } }
+ Timer { *new { arg trig = 0.0;^Timer.performList(trig.rate.rateToSelector, [trig]) } }
+ TIRand { *new { arg lo = 0.0, hi = 127.0, trig = 0.0;^TIRand.performList(trig.rate.rateToSelector, [lo, hi, trig]) } }
+ ToggleFF { *new { arg trig = 0.0;^ToggleFF.performList(trig.rate.rateToSelector, [trig]) } }
+ TRand { *new { arg lo = 0.0, hi = 1.0, trig = 0.0;^TRand.performList(trig.rate.rateToSelector, [lo, hi, trig]) } }
+ Trig { *new { arg in = 0.0, dur = 0.1;^Trig.performList(in.rate.rateToSelector, [in, dur]) } }
+ Trig1 { *new { arg in = 0.0, dur = 0.1;^Trig1.performList(in.rate.rateToSelector, [in, dur]) } }
+ TwoPole { *new { arg in = 0.0, freq = 440.0, radius = 0.8;^TwoPole.performList(in.rate.rateToSelector, [in, freq, radius]) } }
+ TwoZero { *new { arg in = 0.0, freq = 440.0, radius = 0.8;^TwoZero.performList(in.rate.rateToSelector, [in, freq, radius]) } }
+ Wrap { *new { arg in = 0.0, lo = 0.0, hi = 1.0;^Wrap.performList(in.rate.rateToSelector, [in, lo, hi]) } }
+ WrapIndex { *new { arg bufnum = 0.0, in = 0.0;^WrapIndex.performList(in.rate.rateToSelector, [bufnum, in]) } }
+ XFade2 { *new { arg inA = 0.0, inB = 0.0, pan = 0.0, level = 1.0;^XFade2.performList(inA.rate.rateToSelector, [inA, inB, pan, level]) } }
+ ZeroCrossing { *new { arg in = 0.0;^ZeroCrossing.performList(in.rate.rateToSelector, [in]) } }
+ MoogLadder { *new { arg in = 0.0, ffreq = 440.0, res = 0.0;^MoogLadder.performList(in.rate.rateToSelector, [in, ffreq, res]) } }
+ GreyholeRaw { *new { arg in1 = 0.0, in2 = 0.0, damping = 0.0, delaytime = 2.0, diffusion = 0.5, feedback = 0.9, moddepth = 0.1, modfreq = 2.0, size = 1.0;^GreyholeRaw.performList(in1.rate.rateToSelector, [in1, in2, damping, delaytime, diffusion, feedback, moddepth, modfreq, size]) } }
+ CrossoverDistortion { *new { arg in = 0.0, amp = 0.5, smooth = 0.5;^CrossoverDistortion.performList(in.rate.rateToSelector, [in, amp, smooth]) } }
+ RCD { *new { arg clock = 0.0, rotate = 0.0, reset = 0.0, div = 0.0, spread = 0.0, auto = 0.0, len = 0.0, down = 0.0, gates = 0.0;^RCD.performList(clock.rate.rateToSelector, [clock, rotate, reset, div, spread, auto, len, down, gates]) } }
+ TScramble { *new { arg trigger = 0.0, inputs = 0.0;^TScramble.performList(trigger.rate.rateToSelector, [trigger, inputs]) } }
+ ObxdFilter { *new { arg in = 0.0, cutoff = 440.0, resonance = 0.0, multimode = 0.5, bandpass = 0.0, fourpole = 0.0;^ObxdFilter.performList(in.rate.rateToSelector, [in, cutoff, resonance, multimode, bandpass, fourpole]) } }
+ SvfHp { *new { arg in = 0.0, freq = 440.0, q = 0.0;^SvfHp.performList(in.rate.rateToSelector, [in, freq, q]) } }
