+ AmpComp { *new { arg freq = 0.0, root = 0.0, exp = 0.3333; ^AmpComp.multiNew('audio', freq, root, exp) } }
+ AmpCompA { *new { arg freq = 1000.0, root = 0.0, minAmp = 0.32, rootAmp = 1.0; ^AmpCompA.multiNew('audio', freq, root, minAmp, rootAmp) } }
+ Amplitude { *new { arg in = 0.0, attackTime = 1.0e-2, releaseTime = 1.0e-2; ^Amplitude.multiNew('audio', in, attackTime, releaseTime) } }
+ AnalogFoldOsc { *new { arg freq = 100.0, amp = 1.0; ^AnalogFoldOsc.multiNew('audio', freq, amp) } }
+ Blip { *new { arg freq = 440.0, numharm = 200.0; ^Blip.multiNew('audio', freq, numharm) } }
+ BlockSize { *new { ^BlockSize.multiNew('scalar') } }
+ BrownNoise { *new { ^BrownNoise.multiNew('audio') } }
+ BufRd { *new { arg numChannels = 1.0, bufnum = 0.0, phase = 0.0, loop = 1.0, interpolation = 2.0; ^BufRd.multiNew('audio', numChannels, bufnum, phase, loop, interpolation) } }
+ ControlDur { *new { ^ControlDur.multiNew('scalar') } }
+ Crackle { *new { arg chaosParam = 1.5; ^Crackle.multiNew('audio', chaosParam) } }
+ CuspL { *new { arg freq = 22050.0, a = 1.0, b = 1.9, xi = 0.0; ^CuspL.multiNew('audio', freq, a, b, xi) } }
+ Diwhite { *new { arg lo = 0.0, hi = 1.0, length = 1.0e8; ^Diwhite.multiNew('demand', lo, hi, length) } }
+ Drand { *new { arg list = 0.0, repeats = 1.0; ^Drand.multiNew('demand', list, repeats) } }
+ Dseq { *new { arg list = 0.0, repeats = 1.0; ^Dseq.multiNew('demand', list, repeats) } }
+ Dshuf { *new { arg list = 0.0, repeats = 1.0; ^Dshuf.multiNew('demand', list, repeats) } }
+ Dust { *new { arg density = 0.0; ^Dust.multiNew('audio', density) } }
+ Dust2 { *new { arg density = 0.0; ^Dust2.multiNew('audio', density) } }
+ Duty { *new { arg dur = 1.0, reset = 0.0, level = 1.0, doneAction = 0.0; ^Duty.multiNew('audio', dur, reset, level, doneAction) } }
+ EnvGen { *new { arg envelope = 0.0, gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, doneAction = 0.0; ^EnvGen.multiNew('audio', envelope, gate, levelScale, levelBias, timeScale, doneAction) } }
+ Formant { *new { arg fundfreq = 440.0, formfreq = 1760.0, bwfreq = 880.0; ^Formant.multiNew('audio', fundfreq, formfreq, bwfreq) } }
+ FreqShift { *new { arg in = 0.0, freq = 0.0, phase = 0.0; ^FreqShift.multiNew('audio', in, freq, phase) } }
+ FSinOsc { *new { arg freq = 440.0, iphase = 0.0; ^FSinOsc.multiNew('audio', freq, iphase) } }
+ Gendy1 { *new { arg ampdist = 1.0, durdist = 1.0, adparam = 1.0, ddparam = 1.0, minfreq = 440.0, maxfreq = 660.0, ampscale = 0.5, durscale = 0.5, initCPs = 12.0, knum = 0.0; ^Gendy1.multiNew('audio', ampdist, durdist, adparam, ddparam, minfreq, maxfreq, ampscale, durscale, initCPs, knum) } }
+ GrainFM { *new { arg numChannels = 1.0, trigger = 0.0, dur = 1.0, carfreq = 440.0, modfreq = 200.0, index = 1.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0; ^GrainFM.multiNew('audio', numChannels, trigger, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains) } }
+ GrainSin { *new { arg numChannels = 1.0, trigger = 0.0, dur = 1.0, freq = 440.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0; ^GrainSin.multiNew('audio', numChannels, trigger, dur, freq, pan, envbufnum, maxGrains) } }
+ Impulse { *new { arg freq = 440.0, phase = 0.0; ^Impulse.multiNew('audio', freq, phase) } }
+ In { *new { arg numChannels = 1.0, bus = 0.0; ^In.multiNew('audio', numChannels, bus) } }
+ InFeedback { *new { arg numChannels = 1.0, bus = 0.0; ^InFeedback.multiNew('audio', numChannels, bus) } }
+ K2A { *new { arg in = 0.0; ^K2A.multiNew('audio', in) } }
+ Klang { *new { arg specificationsArrayRef = 0.0, freqscale = 1.0, freqoffset = 0.0; ^Klang.multiNew('audio', specificationsArrayRef, freqscale, freqoffset) } }
+ LFCub { *new { arg freq = 440.0, iphase = 0.0; ^LFCub.multiNew('audio', freq, iphase) } }
+ LFDNoise3 { *new { arg freq = 500.0; ^LFDNoise3.multiNew('audio', freq) } }
+ LFGauss { *new { arg duration = 1.0, width = 0.1, iphase = 0.0, loop = 1.0, doneAction = 0.0; ^LFGauss.multiNew('audio', duration, width, iphase, loop, doneAction) } }
+ LFNoise0 { *new { arg freq = 500.0; ^LFNoise0.multiNew('audio', freq) } }
+ LFNoise1 { *new { arg freq = 500.0; ^LFNoise1.multiNew('audio', freq) } }
+ LFNoise2 { *new { arg freq = 500.0; ^LFNoise2.multiNew('audio', freq) } }
+ LFPar { *new { arg freq = 440.0, iphase = 0.0; ^LFPar.multiNew('audio', freq, iphase) } }
+ LFPulse { *new { arg freq = 440.0, iphase = 0.0, width = 0.5; ^LFPulse.multiNew('audio', freq, iphase, width) } }
+ LFSaw { *new { arg freq = 440.0, iphase = 0.0; ^LFSaw.multiNew('audio', freq, iphase) } }
+ LFTri { *new { arg freq = 440.0, iphase = 0.0; ^LFTri.multiNew('audio', freq, iphase) } }
+ Line { *new { arg start = 0.0, end = 1.0, dur = 1.0, doneAction = 0.0; ^Line.multiNew('audio', start, end, dur, doneAction) } }
+ LocalIn { *new { arg numChannels = 1.0, default = 0.0; ^LocalIn.multiNew('audio', numChannels, default) } }
+ MembraneCircle { *new { arg excitation = 0.0, tension = 5.0e-2, loss = 0.99999; ^MembraneCircle.multiNew('audio', excitation, tension, loss) } }
+ MiRings { *new { arg in = 0.0, trig = 0.0, pit = 60.0, struct = 0.25, bright = 0.5, damp = 0.7, pos = 0.25, model = 0.0, poly = 1.0, intern_exciter = 0.0, easteregg = 0.0, bypass = 0.0; ^MiRings.multiNew('audio', in, trig, pit, struct, bright, damp, pos, model, poly, intern_exciter, easteregg, bypass) } }
+ MouseButton { *new { arg minval = 0.0, maxval = 1.0, lag = 0.2; ^MouseButton.multiNew('control', minval, maxval, lag) } }
+ MouseX { *new { arg minval = 0.0, maxval = 1.0, warp = 0.0, lag = 0.2; ^MouseX.multiNew('control', minval, maxval, warp, lag) } }
+ MouseY { *new { arg minval = 0.0, maxval = 1.0, warp = 0.0, lag = 0.2; ^MouseY.multiNew('control', minval, maxval, warp, lag) } }
+ Phasor { *new { arg trig = 0.0, rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0; ^Phasor.multiNew('audio', trig, rate, start, end, resetPos) } }
+ PinkNoise { *new { ^PinkNoise.multiNew('audio') } }
+ Pitch { *new { arg in = 0.0, initFreq = 440.0, minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0, maxBinsPerOctave = 16.0, median = 1.0, ampThreshold = 1.0e-2, peakThreshold = 0.5, downSample = 1.0, clar = 0.0; ^Pitch.multiNew('control', in, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar) } }
+ Pulse { *new { arg freq = 440.0, width = 0.5; ^Pulse.multiNew('audio', freq, width) } }
+ RecordBuf { *new { arg inputArray = 0.0, bufnum = 0.0, offset = 0.0, recLevel = 1.0, preLevel = 0.0, run = 1.0, loop = 1.0, trigger = 1.0, doneAction = 0.0; ^RecordBuf.multiNew('audio', inputArray, bufnum, offset, recLevel, preLevel, run, loop, trigger, doneAction) } }
+ RBezier { *new { arg haltAfter = 100.0, dx = 1.0e-4, freq = 440.0, phase = 0.0, param = 0.0; ^RBezier.multiNew('audio', haltAfter, dx, freq, phase, param) } }
+ RDX7Env { *new { arg gate = 0.0, data = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0, r4 = 0.0, l1 = 0.0, l2 = 0.0, l3 = 0.0, l4 = 0.0, ol = 0.0; ^RDX7Env.multiNew('audio', gate, data, r1, r2, r3, r4, l1, l2, l3, l4, ol) } }
+ RExpRandN { *new { arg numChannels = 1.0, lo = 0.0, hi = 1.0; ^RExpRandN.multiNew('scalar', numChannels, lo, hi) } }
+ RRandN { *new { arg numChannels = 1.0, lo = 0.0, hi = 1.0; ^RRandN.multiNew('scalar', numChannels, lo, hi) } }
+ SampleDur { *new { ^SampleDur.multiNew('scalar') } }
+ SampleRate { *new { ^SampleRate.multiNew('scalar') } }
+ Saw { *new { arg freq = 440.0; ^Saw.multiNew('audio', freq) } }
+ SinOsc { *new { arg freq = 440.0, phase = 0.0; ^SinOsc.multiNew('audio', freq, phase) } }
+ SinOscFB { *new { arg freq = 440.0, feedback = 0.0; ^SinOscFB.multiNew('audio', freq, feedback) } }
+ SyncSaw { *new { arg syncFreq = 440.0, sawFreq = 440.0; ^SyncSaw.multiNew('audio', syncFreq, sawFreq) } }
+ TDuty { *new { arg dur = 1.0, reset = 0.0, level = 1.0, doneAction = 0.0, gapFirst = 0.0; ^TDuty.multiNew('audio', dur, reset, level, doneAction, gapFirst) } }
+ TGrains { *new { arg numChannels = 1.0, trigger = 0.0, bufnum = 0.0, rate = 1.0, centerPos = 0.0, dur = 0.1, pan = 0.0, amp = 0.1, interp = 4.0; ^TGrains.multiNew('audio', numChannels, trigger, bufnum, rate, centerPos, dur, pan, amp, interp) } }
+ VarSaw { *new { arg freq = 440.0, iphase = 0.0, width = 0.5; ^VarSaw.multiNew('audio', freq, iphase, width) } }
+ Vibrato { *new { arg freq = 440.0, rate = 6.0, depth = 2.0e-2, delay = 0.0, onset = 0.0, rateVariation = 4.0e-2, depthVariation = 0.1, iphase = 0.0, trig = 0.0; ^Vibrato.multiNew('audio', freq, rate, depth, delay, onset, rateVariation, depthVariation, iphase, trig) } }
+ WhiteNoise { *new { ^WhiteNoise.multiNew('audio') } }
+ XLine { *new { arg start = 1.0, end = 2.0, dur = 1.0, doneAction = 0.0; ^XLine.multiNew('audio', start, end, dur, doneAction) } }
