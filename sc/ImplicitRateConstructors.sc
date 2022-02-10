+ AmpComp { *new { arg freq = 0.0, root = 0.0, exp = 0.3333; ^AmpComp.performList('audio'.rateToSelector, [freq, root, exp]) } }
+ AmpCompA { *new { arg freq = 1000.0, root = 0.0, minAmp = 0.32, rootAmp = 1.0; ^AmpCompA.performList('audio'.rateToSelector, [freq, root, minAmp, rootAmp]) } }
+ Amplitude { *new { arg in = 0.0, attackTime = 1.0e-2, releaseTime = 1.0e-2; ^Amplitude.performList('audio'.rateToSelector, [in, attackTime, releaseTime]) } }
+ Blip { *new { arg freq = 440.0, numharm = 200.0; ^Blip.performList('audio'.rateToSelector, [freq, numharm]) } }
+ BlockSize { *new { ^BlockSize.performList('scalar'.rateToSelector, []) } }
+ BrownNoise { *new { ^BrownNoise.performList('audio'.rateToSelector, []) } }
+ BufFrames { *new { arg bufnum = 0.0; ^BufFrames.performList('control'.rateToSelector, [bufnum]) } }
+ BufRateScale { *new { arg bufnum = 0.0; ^BufRateScale.performList('control'.rateToSelector, [bufnum]) } }
+ BufRd { *new { arg numChannels = 1.0, bufnum = 0.0, phase = 0.0, loop = 1.0, interpolation = 2.0; ^BufRd.performList('audio'.rateToSelector, [numChannels, bufnum, phase, loop, interpolation]) } }
+ ControlDur { *new { ^ControlDur.performList('scalar'.rateToSelector, []) } }
+ ControlRate { *new { ^ControlRate.performList('scalar'.rateToSelector, []) } }
+ Convolution { *new { arg in = 0.0, kernel = 0.0, framesize = 512.0; ^Convolution.performList('audio'.rateToSelector, [in, kernel, framesize]) } }
+ Crackle { *new { arg chaosParam = 1.5; ^Crackle.performList('audio'.rateToSelector, [chaosParam]) } }
+ CuspL { *new { arg freq = 22050.0, a = 1.0, b = 1.9, xi = 0.0; ^CuspL.performList('audio'.rateToSelector, [freq, a, b, xi]) } }
+ DC { *new { arg in = 0.0; ^DC.performList('audio'.rateToSelector, [in]) } }
+ Dust { *new { arg density = 0.0; ^Dust.performList('audio'.rateToSelector, [density]) } }
+ Dust2 { *new { arg density = 0.0; ^Dust2.performList('audio'.rateToSelector, [density]) } }
+ Duty { *new { arg dur = 1.0, reset = 0.0, level = 1.0, doneAction = 0.0; ^Duty.performList('audio'.rateToSelector, [dur, reset, level, doneAction]) } }
+ EnvGen { *new { arg envelope = 0.0, gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, doneAction = 0.0; ^EnvGen.performList('audio'.rateToSelector, [envelope, gate, levelScale, levelBias, timeScale, doneAction]) } }
+ FBSineC { *new { arg freq = 22050.0, im = 1.0, fb = 0.1, a = 1.1, c = 0.5, xi = 0.1, yi = 0.1; ^FBSineC.performList('audio'.rateToSelector, [freq, im, fb, a, c, xi, yi]) } }
+ Formant { *new { arg fundfreq = 440.0, formfreq = 1760.0, bwfreq = 880.0; ^Formant.performList('audio'.rateToSelector, [fundfreq, formfreq, bwfreq]) } }
+ FreqShift { *new { arg in = 0.0, freq = 0.0, phase = 0.0; ^FreqShift.performList('audio'.rateToSelector, [in, freq, phase]) } }
+ FSinOsc { *new { arg freq = 440.0, iphase = 0.0; ^FSinOsc.performList('audio'.rateToSelector, [freq, iphase]) } }
+ Gendy1 { *new { arg ampdist = 1.0, durdist = 1.0, adparam = 1.0, ddparam = 1.0, minfreq = 440.0, maxfreq = 660.0, ampscale = 0.5, durscale = 0.5, initCPs = 12.0, knum = 0.0; ^Gendy1.performList('audio'.rateToSelector, [ampdist, durdist, adparam, ddparam, minfreq, maxfreq, ampscale, durscale, initCPs, knum]) } }
+ GrainFM { *new { arg numChannels = 1.0, trigger = 0.0, dur = 1.0, carfreq = 440.0, modfreq = 200.0, index = 1.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0; ^GrainFM.performList('audio'.rateToSelector, [numChannels, trigger, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains]) } }
+ GrainSin { *new { arg numChannels = 1.0, trigger = 0.0, dur = 1.0, freq = 440.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0; ^GrainSin.performList('audio'.rateToSelector, [numChannels, trigger, dur, freq, pan, envbufnum, maxGrains]) } }
+ GrayNoise { *new { ^GrayNoise.performList('audio'.rateToSelector, []) } }
+ Impulse { *new { arg freq = 440.0, phase = 0.0; ^Impulse.performList('audio'.rateToSelector, [freq, phase]) } }
+ In { *new { arg numChannels = 1.0, bus = 0.0; ^In.performList('audio'.rateToSelector, [numChannels, bus]) } }
+ K2A { *new { arg in = 0.0; ^K2A.performList('audio'.rateToSelector, [in]) } }
+ KeyState { *new { arg keycode = 0.0, minval = 0.0, maxval = 1.0, lag = 0.2; ^KeyState.performList('control'.rateToSelector, [keycode, minval, maxval, lag]) } }
+ Klang { *new { arg specificationsArrayRef = 0.0, freqscale = 1.0, freqoffset = 0.0; ^Klang.performList('audio'.rateToSelector, [specificationsArrayRef, freqscale, freqoffset]) } }
+ LFCub { *new { arg freq = 440.0, iphase = 0.0; ^LFCub.performList('audio'.rateToSelector, [freq, iphase]) } }
+ LFDNoise1 { *new { arg freq = 500.0; ^LFDNoise1.performList('audio'.rateToSelector, [freq]) } }
+ LFDNoise3 { *new { arg freq = 500.0; ^LFDNoise3.performList('audio'.rateToSelector, [freq]) } }
+ LFGauss { *new { arg duration = 1.0, width = 0.1, iphase = 0.0, loop = 1.0, doneAction = 0.0; ^LFGauss.performList('audio'.rateToSelector, [duration, width, iphase, loop, doneAction]) } }
+ LFNoise0 { *new { arg freq = 500.0; ^LFNoise0.performList('audio'.rateToSelector, [freq]) } }
+ LFNoise1 { *new { arg freq = 500.0; ^LFNoise1.performList('audio'.rateToSelector, [freq]) } }
+ LFNoise2 { *new { arg freq = 500.0; ^LFNoise2.performList('audio'.rateToSelector, [freq]) } }
+ LFPar { *new { arg freq = 440.0, iphase = 0.0; ^LFPar.performList('audio'.rateToSelector, [freq, iphase]) } }
+ LFPulse { *new { arg freq = 440.0, iphase = 0.0, width = 0.5; ^LFPulse.performList('audio'.rateToSelector, [freq, iphase, width]) } }
+ LFSaw { *new { arg freq = 440.0, iphase = 0.0; ^LFSaw.performList('audio'.rateToSelector, [freq, iphase]) } }
+ LFTri { *new { arg freq = 440.0, iphase = 0.0; ^LFTri.performList('audio'.rateToSelector, [freq, iphase]) } }
+ Line { *new { arg start = 0.0, end = 1.0, dur = 1.0, doneAction = 0.0; ^Line.performList('audio'.rateToSelector, [start, end, dur, doneAction]) } }
+ LocalIn { *new { arg numChannels = 1.0, default = 0.0; ^LocalIn.performList('audio'.rateToSelector, [numChannels, default]) } }
+ MouseButton { *new { arg minval = 0.0, maxval = 1.0, lag = 0.2; ^MouseButton.performList('control'.rateToSelector, [minval, maxval, lag]) } }
+ MouseX { *new { arg minval = 0.0, maxval = 1.0, warp = 0.0, lag = 0.2; ^MouseX.performList('control'.rateToSelector, [minval, maxval, warp, lag]) } }
+ MouseY { *new { arg minval = 0.0, maxval = 1.0, warp = 0.0, lag = 0.2; ^MouseY.performList('control'.rateToSelector, [minval, maxval, warp, lag]) } }
+ NRand { *new { arg lo = 0.0, hi = 1.0, n = 0.0; ^NRand.performList('scalar'.rateToSelector, [lo, hi, n]) } }
+ NumOutputBuses { *new { ^NumOutputBuses.performList('scalar'.rateToSelector, []) } }
+ Osc { *new { arg bufnum = 0.0, freq = 440.0, phase = 0.0; ^Osc.performList('audio'.rateToSelector, [bufnum, freq, phase]) } }
+ Phasor { *new { arg trig = 0.0, rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0; ^Phasor.performList('audio'.rateToSelector, [trig, rate, start, end, resetPos]) } }
+ PinkNoise { *new { ^PinkNoise.performList('audio'.rateToSelector, []) } }
+ Pitch { *new { arg in = 0.0, initFreq = 440.0, minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0, maxBinsPerOctave = 16.0, median = 1.0, ampThreshold = 1.0e-2, peakThreshold = 0.5, downSample = 1.0, clar = 0.0; ^Pitch.performList('control'.rateToSelector, [in, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar]) } }
+ PlayBuf { *new { arg numChannels = 1.0, bufnum = 0.0, rate = 1.0, trigger = 1.0, startPos = 0.0, loop = 0.0, doneAction = 0.0; ^PlayBuf.performList('audio'.rateToSelector, [numChannels, bufnum, rate, trigger, startPos, loop, doneAction]) } }
+ Pulse { *new { arg freq = 440.0, width = 0.5; ^Pulse.performList('audio'.rateToSelector, [freq, width]) } }
+ QuadC { *new { arg freq = 22050.0, a = 1.0, b = -1.0, c = -0.75, xi = 0.0; ^QuadC.performList('audio'.rateToSelector, [freq, a, b, c, xi]) } }
+ RecordBuf { *new { arg inputArray = 0.0, bufnum = 0.0, offset = 0.0, recLevel = 1.0, preLevel = 0.0, run = 1.0, loop = 1.0, trigger = 1.0, doneAction = 0.0; ^RecordBuf.performList('audio'.rateToSelector, [inputArray, bufnum, offset, recLevel, preLevel, run, loop, trigger, doneAction]) } }
+ SampleDur { *new { ^SampleDur.performList('scalar'.rateToSelector, []) } }
+ SampleRate { *new { ^SampleRate.performList('scalar'.rateToSelector, []) } }
+ Saw { *new { arg freq = 440.0; ^Saw.performList('audio'.rateToSelector, [freq]) } }
+ SinOsc { *new { arg freq = 440.0, phase = 0.0; ^SinOsc.performList('audio'.rateToSelector, [freq, phase]) } }
+ SinOscFB { *new { arg freq = 440.0, feedback = 0.0; ^SinOscFB.performList('audio'.rateToSelector, [freq, feedback]) } }
+ SyncSaw { *new { arg syncFreq = 440.0, sawFreq = 440.0; ^SyncSaw.performList('audio'.rateToSelector, [syncFreq, sawFreq]) } }
+ TDuty { *new { arg dur = 1.0, reset = 0.0, level = 1.0, doneAction = 0.0, gapFirst = 0.0; ^TDuty.performList('audio'.rateToSelector, [dur, reset, level, doneAction, gapFirst]) } }
+ TGrains { *new { arg numChannels = 1.0, trigger = 0.0, bufnum = 0.0, rate = 1.0, centerPos = 0.0, dur = 0.1, pan = 0.0, amp = 0.1, interp = 4.0; ^TGrains.performList('audio'.rateToSelector, [numChannels, trigger, bufnum, rate, centerPos, dur, pan, amp, interp]) } }
+ VarSaw { *new { arg freq = 440.0, iphase = 0.0, width = 0.5; ^VarSaw.performList('audio'.rateToSelector, [freq, iphase, width]) } }
+ Vibrato { *new { arg freq = 440.0, rate = 6.0, depth = 2.0e-2, delay = 0.0, onset = 0.0, rateVariation = 4.0e-2, depthVariation = 0.1, iphase = 0.0, trig = 0.0; ^Vibrato.performList('audio'.rateToSelector, [freq, rate, depth, delay, onset, rateVariation, depthVariation, iphase, trig]) } }
+ WhiteNoise { *new { ^WhiteNoise.performList('audio'.rateToSelector, []) } }
+ XLine { *new { arg start = 1.0, end = 2.0, dur = 1.0, doneAction = 0.0; ^XLine.performList('audio'.rateToSelector, [start, end, dur, doneAction]) } }
+ Friction { *new { arg in = 0.0, friction = 0.5, spring = 0.414, damp = 0.313, mass = 0.1, beltmass = 1.0; ^Friction.performList('audio'.rateToSelector, [in, friction, spring, damp, mass, beltmass]) } }
+ MembraneCircle { *new { arg excitation = 0.0, tension = 5.0e-2, loss = 0.99999; ^MembraneCircle.performList('audio'.rateToSelector, [excitation, tension, loss]) } }
+ MiRings { *new { arg in = 0.0, trig = 0.0, pit = 60.0, struct = 0.25, bright = 0.5, damp = 0.7, pos = 0.25, model = 0.0, poly = 1.0, intern_exciter = 0.0, easteregg = 0.0, bypass = 0.0; ^MiRings.performList('audio'.rateToSelector, [in, trig, pit, struct, bright, damp, pos, model, poly, intern_exciter, easteregg, bypass]) } }
+ AnalogFoldOsc { *new { arg freq = 100.0, amp = 1.0; ^AnalogFoldOsc.performList('audio'.rateToSelector, [freq, amp]) } }
+ SCM { *new { arg clock = 0.0, bpm = 120.0, rotate = 0.0, slip = 0.0, shuffle = 0.0, skip = 0.0, pw = 0.0; ^SCM.performList('audio'.rateToSelector, [clock, bpm, rotate, slip, shuffle, skip, pw]) } }
+ ExpRandN { *new { arg numChannels = 1.0, lo = 0.0, hi = 1.0; ^ExpRandN.performList('scalar'.rateToSelector, [numChannels, lo, hi]) } }
+ LinRandN { *new { arg numChannels = 1.0, lo = 0.0, hi = 1.0, minmax = 0.0; ^LinRandN.performList('scalar'.rateToSelector, [numChannels, lo, hi, minmax]) } }
+ RandN { *new { arg numChannels = 1.0, lo = 0.0, hi = 1.0; ^RandN.performList('scalar'.rateToSelector, [numChannels, lo, hi]) } }
+ RBezier { *new { arg haltAfter = 100.0, dx = 1.0e-4, freq = 440.0, phase = 0.0, param = 0.0; ^RBezier.performList('audio'.rateToSelector, [haltAfter, dx, freq, phase, param]) } }
+ RDX7 { *new { arg bufnum = 0.0, on = 0.0, off = 0.0, data = 0.0, vc = 0.0, mnn = 60.0, vel = 99.0, pw = 0.0, mw = 0.0, bc = 0.0, fc = 0.0; ^RDX7.performList('audio'.rateToSelector, [bufnum, on, off, data, vc, mnn, vel, pw, mw, bc, fc]) } }
+ RDX7Env { *new { arg gate = 0.0, data = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0, r4 = 0.0, l1 = 0.0, l2 = 0.0, l3 = 0.0, l4 = 0.0, ol = 0.0; ^RDX7Env.performList('audio'.rateToSelector, [gate, data, r1, r2, r3, r4, l1, l2, l3, l4, ol]) } }
