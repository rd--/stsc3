+ UGen { ampComp { arg root = 0.0, exp = 0.3333;  ^AmpComp.multiNew('audio', this, root, exp) } }
+ UGen { ampCompA { arg root = 0.0, minAmp = 0.32, rootAmp = 1.0;  ^AmpCompA.multiNew('audio', this, root, minAmp, rootAmp) } }
+ UGen { amplitude { arg attackTime = 1.0e-2, releaseTime = 1.0e-2;  ^Amplitude.multiNew('audio', this, attackTime, releaseTime) } }
+ UGen { blip { arg numharm = 200.0;  ^Blip.multiNew('audio', this, numharm) } }
+ UGen { bufRd { arg phase = 0.0, loop = 1.0, interpolation = 2.0;  ^BufRd.multiNew('audio', this, phase, loop, interpolation) } }
+ UGen { crackle {  ^Crackle.multiNew('audio', this) } }
+ UGen { cuspL { arg a = 1.0, b = 1.9, xi = 0.0;  ^CuspL.multiNew('audio', this, a, b, xi) } }
+ UGen { diwhite { arg hi = 1.0, length = 1.0e8;  ^Diwhite.multiNew('demand', this, hi, length) } }
+ UGen { drand { arg repeats = 1.0;  ^Drand.multiNew('demand', this, repeats) } }
+ UGen { dseq { arg repeats = 1.0;  ^Dseq.multiNew('demand', this, repeats) } }
+ UGen { dshuf { arg repeats = 1.0;  ^Dshuf.multiNew('demand', this, repeats) } }
+ UGen { dust {  ^Dust.multiNew('audio', this) } }
+ UGen { dust2 {  ^Dust2.multiNew('audio', this) } }
+ UGen { duty { arg reset = 0.0, level = 1.0, doneAction = 0.0;  ^Duty.multiNew('audio', this, reset, level, doneAction) } }
+ UGen { envGen { arg gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, doneAction = 0.0;  ^EnvGen.multiNew('audio', this, gate, levelScale, levelBias, timeScale, doneAction) } }
+ UGen { formant { arg formfreq = 1760.0, bwfreq = 880.0;  ^Formant.multiNew('audio', this, formfreq, bwfreq) } }
+ UGen { fSinOsc { arg iphase = 0.0;  ^FSinOsc.multiNew('audio', this, iphase) } }
+ UGen { gendy1 { arg durdist = 1.0, adparam = 1.0, ddparam = 1.0, minfreq = 440.0, maxfreq = 660.0, ampscale = 0.5, durscale = 0.5, initCPs = 12.0, knum = 0.0;  ^Gendy1.multiNew('audio', this, durdist, adparam, ddparam, minfreq, maxfreq, ampscale, durscale, initCPs, knum) } }
+ UGen { grainFM { arg dur = 1.0, carfreq = 440.0, modfreq = 200.0, index = 1.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainFM.multiNew('audio', this, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains) } }
+ UGen { grainSin { arg dur = 1.0, freq = 440.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainSin.multiNew('audio', this, dur, freq, pan, envbufnum, maxGrains) } }
+ UGen { impulse { arg phase = 0.0;  ^Impulse.multiNew('audio', this, phase) } }
+ UGen { in {  ^In.multiNew('audio', this) } }
+ UGen { inFeedback {  ^InFeedback.multiNew('audio', this) } }
+ UGen { k2a {  ^K2A.multiNew('audio', this) } }
+ UGen { klang { arg freqscale = 1.0, freqoffset = 0.0;  ^Klang.multiNew('audio', this, freqscale, freqoffset) } }
+ UGen { lfCub { arg iphase = 0.0;  ^LFCub.multiNew('audio', this, iphase) } }
+ UGen { lfdNoise3 {  ^LFDNoise3.multiNew('audio', this) } }
+ UGen { lfNoise0 {  ^LFNoise0.multiNew('audio', this) } }
+ UGen { lfNoise1 {  ^LFNoise1.multiNew('audio', this) } }
+ UGen { lfNoise2 {  ^LFNoise2.multiNew('audio', this) } }
+ UGen { lfPar { arg iphase = 0.0;  ^LFPar.multiNew('audio', this, iphase) } }
+ UGen { lfPulse { arg iphase = 0.0, width = 0.5;  ^LFPulse.multiNew('audio', this, iphase, width) } }
+ UGen { lfSaw { arg iphase = 0.0;  ^LFSaw.multiNew('audio', this, iphase) } }
+ UGen { lfTri { arg iphase = 0.0;  ^LFTri.multiNew('audio', this, iphase) } }
+ UGen { line { arg end = 1.0, dur = 1.0, doneAction = 0.0;  ^Line.multiNew('audio', this, end, dur, doneAction) } }
+ UGen { localIn {  ^LocalIn.multiNew('audio', this) } }
+ UGen { membraneCircle { arg tension = 5.0e-2, loss = 0.99999;  ^MembraneCircle.multiNew('audio', this, tension, loss) } }
+ UGen { miRings { arg trig = 0.0, pit = 60.0, struct = 0.25, bright = 0.5, damp = 0.7, pos = 0.25, model = 0.0, poly = 1.0, intern_exciter = 0.0, easteregg = 0.0, bypass = 0.0;  ^MiRings.multiNew('audio', this, trig, pit, struct, bright, damp, pos, model, poly, intern_exciter, easteregg, bypass) } }
+ UGen { mouseButton { arg maxval = 1.0, lag = 0.2;  ^MouseButton.multiNew('control', this, maxval, lag) } }
+ UGen { mouseX { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseX.multiNew('control', this, maxval, warp, lag) } }
+ UGen { mouseY { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseY.multiNew('control', this, maxval, warp, lag) } }
+ UGen { phasor { arg rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0;  ^Phasor.multiNew('audio', this, rate, start, end, resetPos) } }
+ UGen { pitch { arg initFreq = 440.0, minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0, maxBinsPerOctave = 16.0, median = 1.0, ampThreshold = 1.0e-2, peakThreshold = 0.5, downSample = 1.0, clar = 0.0;  ^Pitch.multiNew('control', this, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar) } }
+ UGen { pulse { arg width = 0.5;  ^Pulse.multiNew('audio', this, width) } }
+ UGen { recordBuf { arg bufnum = 0.0, offset = 0.0, recLevel = 1.0, preLevel = 0.0, run = 1.0, loop = 1.0, trigger = 1.0, doneAction = 0.0;  ^RecordBuf.multiNew('audio', this, bufnum, offset, recLevel, preLevel, run, loop, trigger, doneAction) } }
+ UGen { rBezier { arg dx = 1.0e-4, freq = 440.0, phase = 0.0, param = 0.0;  ^RBezier.multiNew('audio', this, dx, freq, phase, param) } }
+ UGen { rdx7Env { arg data = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0, r4 = 0.0, l1 = 0.0, l2 = 0.0, l3 = 0.0, l4 = 0.0, ol = 0.0;  ^RDX7Env.multiNew('audio', this, data, r1, r2, r3, r4, l1, l2, l3, l4, ol) } }
+ UGen { rExpRandN { arg hi = 1.0;  ^RExpRandN.multiNew('scalar', this, hi) } }
+ UGen { rRandN { arg hi = 1.0;  ^RRandN.multiNew('scalar', this, hi) } }
+ UGen { saw {  ^Saw.multiNew('audio', this) } }
+ UGen { sinOsc { arg phase = 0.0;  ^SinOsc.multiNew('audio', this, phase) } }
+ UGen { sinOscFB { arg feedback = 0.0;  ^SinOscFB.multiNew('audio', this, feedback) } }
+ UGen { syncSaw { arg sawFreq = 440.0;  ^SyncSaw.multiNew('audio', this, sawFreq) } }
+ UGen { tDuty { arg reset = 0.0, level = 1.0, doneAction = 0.0, gapFirst = 0.0;  ^TDuty.multiNew('audio', this, reset, level, doneAction, gapFirst) } }
+ UGen { tGrains { arg bufnum = 0.0, rate = 1.0, centerPos = 0.0, dur = 0.1, pan = 0.0, amp = 0.1, interp = 4.0;  ^TGrains.multiNew('audio', this, bufnum, rate, centerPos, dur, pan, amp, interp) } }
+ UGen { varSaw { arg iphase = 0.0, width = 0.5;  ^VarSaw.multiNew('audio', this, iphase, width) } }
+ UGen { vibrato { arg rate = 6.0, depth = 2.0e-2, delay = 0.0, onset = 0.0, rateVariation = 4.0e-2, depthVariation = 0.1, iphase = 0.0, trig = 0.0;  ^Vibrato.multiNew('audio', this, rate, depth, delay, onset, rateVariation, depthVariation, iphase, trig) } }
+ UGen { xLine { arg end = 2.0, dur = 1.0, doneAction = 0.0;  ^XLine.multiNew('audio', this, end, dur, doneAction) } }

+ Array { ampComp { arg root = 0.0, exp = 0.3333;  ^AmpComp.multiNew('audio', this, root, exp) } }
+ Array { ampCompA { arg root = 0.0, minAmp = 0.32, rootAmp = 1.0;  ^AmpCompA.multiNew('audio', this, root, minAmp, rootAmp) } }
+ Array { amplitude { arg attackTime = 1.0e-2, releaseTime = 1.0e-2;  ^Amplitude.multiNew('audio', this, attackTime, releaseTime) } }
+ Array { blip { arg numharm = 200.0;  ^Blip.multiNew('audio', this, numharm) } }
+ Array { bufRd { arg phase = 0.0, loop = 1.0, interpolation = 2.0;  ^BufRd.multiNew('audio', this, phase, loop, interpolation) } }
+ Array { crackle {  ^Crackle.multiNew('audio', this) } }
+ Array { cuspL { arg a = 1.0, b = 1.9, xi = 0.0;  ^CuspL.multiNew('audio', this, a, b, xi) } }
+ Array { diwhite { arg hi = 1.0, length = 1.0e8;  ^Diwhite.multiNew('demand', this, hi, length) } }
+ Array { drand { arg repeats = 1.0;  ^Drand.multiNew('demand', this, repeats) } }
+ Array { dseq { arg repeats = 1.0;  ^Dseq.multiNew('demand', this, repeats) } }
+ Array { dshuf { arg repeats = 1.0;  ^Dshuf.multiNew('demand', this, repeats) } }
+ Array { dust {  ^Dust.multiNew('audio', this) } }
+ Array { dust2 {  ^Dust2.multiNew('audio', this) } }
+ Array { duty { arg reset = 0.0, level = 1.0, doneAction = 0.0;  ^Duty.multiNew('audio', this, reset, level, doneAction) } }
+ Array { envGen { arg gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, doneAction = 0.0;  ^EnvGen.multiNew('audio', this, gate, levelScale, levelBias, timeScale, doneAction) } }
+ Array { formant { arg formfreq = 1760.0, bwfreq = 880.0;  ^Formant.multiNew('audio', this, formfreq, bwfreq) } }
+ Array { fSinOsc { arg iphase = 0.0;  ^FSinOsc.multiNew('audio', this, iphase) } }
+ Array { gendy1 { arg durdist = 1.0, adparam = 1.0, ddparam = 1.0, minfreq = 440.0, maxfreq = 660.0, ampscale = 0.5, durscale = 0.5, initCPs = 12.0, knum = 0.0;  ^Gendy1.multiNew('audio', this, durdist, adparam, ddparam, minfreq, maxfreq, ampscale, durscale, initCPs, knum) } }
+ Array { grainFM { arg dur = 1.0, carfreq = 440.0, modfreq = 200.0, index = 1.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainFM.multiNew('audio', this, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains) } }
+ Array { grainSin { arg dur = 1.0, freq = 440.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainSin.multiNew('audio', this, dur, freq, pan, envbufnum, maxGrains) } }
+ Array { impulse { arg phase = 0.0;  ^Impulse.multiNew('audio', this, phase) } }
+ Array { in {  ^In.multiNew('audio', this) } }
+ Array { inFeedback {  ^InFeedback.multiNew('audio', this) } }
+ Array { k2a {  ^K2A.multiNew('audio', this) } }
+ Array { klang { arg freqscale = 1.0, freqoffset = 0.0;  ^Klang.multiNew('audio', this, freqscale, freqoffset) } }
+ Array { lfCub { arg iphase = 0.0;  ^LFCub.multiNew('audio', this, iphase) } }
+ Array { lfdNoise3 {  ^LFDNoise3.multiNew('audio', this) } }
+ Array { lfNoise0 {  ^LFNoise0.multiNew('audio', this) } }
+ Array { lfNoise1 {  ^LFNoise1.multiNew('audio', this) } }
+ Array { lfNoise2 {  ^LFNoise2.multiNew('audio', this) } }
+ Array { lfPar { arg iphase = 0.0;  ^LFPar.multiNew('audio', this, iphase) } }
+ Array { lfPulse { arg iphase = 0.0, width = 0.5;  ^LFPulse.multiNew('audio', this, iphase, width) } }
+ Array { lfSaw { arg iphase = 0.0;  ^LFSaw.multiNew('audio', this, iphase) } }
+ Array { lfTri { arg iphase = 0.0;  ^LFTri.multiNew('audio', this, iphase) } }
+ Array { line { arg end = 1.0, dur = 1.0, doneAction = 0.0;  ^Line.multiNew('audio', this, end, dur, doneAction) } }
+ Array { localIn {  ^LocalIn.multiNew('audio', this) } }
+ Array { membraneCircle { arg tension = 5.0e-2, loss = 0.99999;  ^MembraneCircle.multiNew('audio', this, tension, loss) } }
+ Array { miRings { arg trig = 0.0, pit = 60.0, struct = 0.25, bright = 0.5, damp = 0.7, pos = 0.25, model = 0.0, poly = 1.0, intern_exciter = 0.0, easteregg = 0.0, bypass = 0.0;  ^MiRings.multiNew('audio', this, trig, pit, struct, bright, damp, pos, model, poly, intern_exciter, easteregg, bypass) } }
+ Array { mouseButton { arg maxval = 1.0, lag = 0.2;  ^MouseButton.multiNew('control', this, maxval, lag) } }
+ Array { mouseX { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseX.multiNew('control', this, maxval, warp, lag) } }
+ Array { mouseY { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseY.multiNew('control', this, maxval, warp, lag) } }
+ Array { phasor { arg rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0;  ^Phasor.multiNew('audio', this, rate, start, end, resetPos) } }
+ Array { pitch { arg initFreq = 440.0, minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0, maxBinsPerOctave = 16.0, median = 1.0, ampThreshold = 1.0e-2, peakThreshold = 0.5, downSample = 1.0, clar = 0.0;  ^Pitch.multiNew('control', this, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar) } }
+ Array { pulse { arg width = 0.5;  ^Pulse.multiNew('audio', this, width) } }
+ Array { recordBuf { arg bufnum = 0.0, offset = 0.0, recLevel = 1.0, preLevel = 0.0, run = 1.0, loop = 1.0, trigger = 1.0, doneAction = 0.0;  ^RecordBuf.multiNew('audio', this, bufnum, offset, recLevel, preLevel, run, loop, trigger, doneAction) } }
+ Array { rBezier { arg dx = 1.0e-4, freq = 440.0, phase = 0.0, param = 0.0;  ^RBezier.multiNew('audio', this, dx, freq, phase, param) } }
+ Array { rdx7Env { arg data = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0, r4 = 0.0, l1 = 0.0, l2 = 0.0, l3 = 0.0, l4 = 0.0, ol = 0.0;  ^RDX7Env.multiNew('audio', this, data, r1, r2, r3, r4, l1, l2, l3, l4, ol) } }
+ Array { rExpRandN { arg hi = 1.0;  ^RExpRandN.multiNew('scalar', this, hi) } }
+ Array { rRandN { arg hi = 1.0;  ^RRandN.multiNew('scalar', this, hi) } }
+ Array { saw {  ^Saw.multiNew('audio', this) } }
+ Array { sinOsc { arg phase = 0.0;  ^SinOsc.multiNew('audio', this, phase) } }
+ Array { sinOscFB { arg feedback = 0.0;  ^SinOscFB.multiNew('audio', this, feedback) } }
+ Array { syncSaw { arg sawFreq = 440.0;  ^SyncSaw.multiNew('audio', this, sawFreq) } }
+ Array { tDuty { arg reset = 0.0, level = 1.0, doneAction = 0.0, gapFirst = 0.0;  ^TDuty.multiNew('audio', this, reset, level, doneAction, gapFirst) } }
+ Array { tGrains { arg bufnum = 0.0, rate = 1.0, centerPos = 0.0, dur = 0.1, pan = 0.0, amp = 0.1, interp = 4.0;  ^TGrains.multiNew('audio', this, bufnum, rate, centerPos, dur, pan, amp, interp) } }
+ Array { varSaw { arg iphase = 0.0, width = 0.5;  ^VarSaw.multiNew('audio', this, iphase, width) } }
+ Array { vibrato { arg rate = 6.0, depth = 2.0e-2, delay = 0.0, onset = 0.0, rateVariation = 4.0e-2, depthVariation = 0.1, iphase = 0.0, trig = 0.0;  ^Vibrato.multiNew('audio', this, rate, depth, delay, onset, rateVariation, depthVariation, iphase, trig) } }
+ Array { xLine { arg end = 2.0, dur = 1.0, doneAction = 0.0;  ^XLine.multiNew('audio', this, end, dur, doneAction) } }

+ SimpleNumber { ampComp { arg root = 0.0, exp = 0.3333;  ^AmpComp.multiNew('audio', this, root, exp) } }
+ SimpleNumber { ampCompA { arg root = 0.0, minAmp = 0.32, rootAmp = 1.0;  ^AmpCompA.multiNew('audio', this, root, minAmp, rootAmp) } }
+ SimpleNumber { amplitude { arg attackTime = 1.0e-2, releaseTime = 1.0e-2;  ^Amplitude.multiNew('audio', this, attackTime, releaseTime) } }
+ SimpleNumber { blip { arg numharm = 200.0;  ^Blip.multiNew('audio', this, numharm) } }
+ SimpleNumber { bufRd { arg phase = 0.0, loop = 1.0, interpolation = 2.0;  ^BufRd.multiNew('audio', this, phase, loop, interpolation) } }
+ SimpleNumber { crackle {  ^Crackle.multiNew('audio', this) } }
+ SimpleNumber { cuspL { arg a = 1.0, b = 1.9, xi = 0.0;  ^CuspL.multiNew('audio', this, a, b, xi) } }
+ SimpleNumber { diwhite { arg hi = 1.0, length = 1.0e8;  ^Diwhite.multiNew('demand', this, hi, length) } }
+ SimpleNumber { drand { arg repeats = 1.0;  ^Drand.multiNew('demand', this, repeats) } }
+ SimpleNumber { dseq { arg repeats = 1.0;  ^Dseq.multiNew('demand', this, repeats) } }
+ SimpleNumber { dshuf { arg repeats = 1.0;  ^Dshuf.multiNew('demand', this, repeats) } }
+ SimpleNumber { dust {  ^Dust.multiNew('audio', this) } }
+ SimpleNumber { dust2 {  ^Dust2.multiNew('audio', this) } }
+ SimpleNumber { duty { arg reset = 0.0, level = 1.0, doneAction = 0.0;  ^Duty.multiNew('audio', this, reset, level, doneAction) } }
+ SimpleNumber { envGen { arg gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, doneAction = 0.0;  ^EnvGen.multiNew('audio', this, gate, levelScale, levelBias, timeScale, doneAction) } }
+ SimpleNumber { formant { arg formfreq = 1760.0, bwfreq = 880.0;  ^Formant.multiNew('audio', this, formfreq, bwfreq) } }
+ SimpleNumber { fSinOsc { arg iphase = 0.0;  ^FSinOsc.multiNew('audio', this, iphase) } }
+ SimpleNumber { gendy1 { arg durdist = 1.0, adparam = 1.0, ddparam = 1.0, minfreq = 440.0, maxfreq = 660.0, ampscale = 0.5, durscale = 0.5, initCPs = 12.0, knum = 0.0;  ^Gendy1.multiNew('audio', this, durdist, adparam, ddparam, minfreq, maxfreq, ampscale, durscale, initCPs, knum) } }
+ SimpleNumber { grainFM { arg dur = 1.0, carfreq = 440.0, modfreq = 200.0, index = 1.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainFM.multiNew('audio', this, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains) } }
+ SimpleNumber { grainSin { arg dur = 1.0, freq = 440.0, pan = 0.0, envbufnum = -1.0, maxGrains = 512.0;  ^GrainSin.multiNew('audio', this, dur, freq, pan, envbufnum, maxGrains) } }
+ SimpleNumber { impulse { arg phase = 0.0;  ^Impulse.multiNew('audio', this, phase) } }
+ SimpleNumber { in {  ^In.multiNew('audio', this) } }
+ SimpleNumber { inFeedback {  ^InFeedback.multiNew('audio', this) } }
+ SimpleNumber { k2a {  ^K2A.multiNew('audio', this) } }
+ SimpleNumber { klang { arg freqscale = 1.0, freqoffset = 0.0;  ^Klang.multiNew('audio', this, freqscale, freqoffset) } }
+ SimpleNumber { lfCub { arg iphase = 0.0;  ^LFCub.multiNew('audio', this, iphase) } }
+ SimpleNumber { lfdNoise3 {  ^LFDNoise3.multiNew('audio', this) } }
+ SimpleNumber { lfNoise0 {  ^LFNoise0.multiNew('audio', this) } }
+ SimpleNumber { lfNoise1 {  ^LFNoise1.multiNew('audio', this) } }
+ SimpleNumber { lfNoise2 {  ^LFNoise2.multiNew('audio', this) } }
+ SimpleNumber { lfPar { arg iphase = 0.0;  ^LFPar.multiNew('audio', this, iphase) } }
+ SimpleNumber { lfPulse { arg iphase = 0.0, width = 0.5;  ^LFPulse.multiNew('audio', this, iphase, width) } }
+ SimpleNumber { lfSaw { arg iphase = 0.0;  ^LFSaw.multiNew('audio', this, iphase) } }
+ SimpleNumber { lfTri { arg iphase = 0.0;  ^LFTri.multiNew('audio', this, iphase) } }
+ SimpleNumber { line { arg end = 1.0, dur = 1.0, doneAction = 0.0;  ^Line.multiNew('audio', this, end, dur, doneAction) } }
+ SimpleNumber { localIn {  ^LocalIn.multiNew('audio', this) } }
+ SimpleNumber { membraneCircle { arg tension = 5.0e-2, loss = 0.99999;  ^MembraneCircle.multiNew('audio', this, tension, loss) } }
+ SimpleNumber { miRings { arg trig = 0.0, pit = 60.0, struct = 0.25, bright = 0.5, damp = 0.7, pos = 0.25, model = 0.0, poly = 1.0, intern_exciter = 0.0, easteregg = 0.0, bypass = 0.0;  ^MiRings.multiNew('audio', this, trig, pit, struct, bright, damp, pos, model, poly, intern_exciter, easteregg, bypass) } }
+ SimpleNumber { mouseButton { arg maxval = 1.0, lag = 0.2;  ^MouseButton.multiNew('control', this, maxval, lag) } }
+ SimpleNumber { mouseX { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseX.multiNew('control', this, maxval, warp, lag) } }
+ SimpleNumber { mouseY { arg maxval = 1.0, warp = 0.0, lag = 0.2;  ^MouseY.multiNew('control', this, maxval, warp, lag) } }
+ SimpleNumber { phasor { arg rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0;  ^Phasor.multiNew('audio', this, rate, start, end, resetPos) } }
+ SimpleNumber { pitch { arg initFreq = 440.0, minFreq = 60.0, maxFreq = 4000.0, execFreq = 100.0, maxBinsPerOctave = 16.0, median = 1.0, ampThreshold = 1.0e-2, peakThreshold = 0.5, downSample = 1.0, clar = 0.0;  ^Pitch.multiNew('control', this, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample, clar) } }
+ SimpleNumber { pulse { arg width = 0.5;  ^Pulse.multiNew('audio', this, width) } }
+ SimpleNumber { recordBuf { arg bufnum = 0.0, offset = 0.0, recLevel = 1.0, preLevel = 0.0, run = 1.0, loop = 1.0, trigger = 1.0, doneAction = 0.0;  ^RecordBuf.multiNew('audio', this, bufnum, offset, recLevel, preLevel, run, loop, trigger, doneAction) } }
+ SimpleNumber { rBezier { arg dx = 1.0e-4, freq = 440.0, phase = 0.0, param = 0.0;  ^RBezier.multiNew('audio', this, dx, freq, phase, param) } }
+ SimpleNumber { rdx7Env { arg data = 0.0, r1 = 0.0, r2 = 0.0, r3 = 0.0, r4 = 0.0, l1 = 0.0, l2 = 0.0, l3 = 0.0, l4 = 0.0, ol = 0.0;  ^RDX7Env.multiNew('audio', this, data, r1, r2, r3, r4, l1, l2, l3, l4, ol) } }
+ SimpleNumber { rExpRandN { arg hi = 1.0;  ^RExpRandN.multiNew('scalar', this, hi) } }
+ SimpleNumber { rRandN { arg hi = 1.0;  ^RRandN.multiNew('scalar', this, hi) } }
+ SimpleNumber { saw {  ^Saw.multiNew('audio', this) } }
+ SimpleNumber { sinOsc { arg phase = 0.0;  ^SinOsc.multiNew('audio', this, phase) } }
+ SimpleNumber { sinOscFB { arg feedback = 0.0;  ^SinOscFB.multiNew('audio', this, feedback) } }
+ SimpleNumber { syncSaw { arg sawFreq = 440.0;  ^SyncSaw.multiNew('audio', this, sawFreq) } }
+ SimpleNumber { tDuty { arg reset = 0.0, level = 1.0, doneAction = 0.0, gapFirst = 0.0;  ^TDuty.multiNew('audio', this, reset, level, doneAction, gapFirst) } }
+ SimpleNumber { tGrains { arg bufnum = 0.0, rate = 1.0, centerPos = 0.0, dur = 0.1, pan = 0.0, amp = 0.1, interp = 4.0;  ^TGrains.multiNew('audio', this, bufnum, rate, centerPos, dur, pan, amp, interp) } }
+ SimpleNumber { varSaw { arg iphase = 0.0, width = 0.5;  ^VarSaw.multiNew('audio', this, iphase, width) } }
+ SimpleNumber { vibrato { arg rate = 6.0, depth = 2.0e-2, delay = 0.0, onset = 0.0, rateVariation = 4.0e-2, depthVariation = 0.1, iphase = 0.0, trig = 0.0;  ^Vibrato.multiNew('audio', this, rate, depth, delay, onset, rateVariation, depthVariation, iphase, trig) } }
+ SimpleNumber { xLine { arg end = 2.0, dur = 1.0, doneAction = 0.0;  ^XLine.multiNew('audio', this, end, dur, doneAction) } }

