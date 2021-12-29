+ UGen {
  allpassC {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  allpassL {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassL.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  allpassN {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  balance2 {
    arg right = 0.0, pos = 0.0, level = 1.0;
    ^Balance2.performList(this.rate.rateToSelector, [this, right, pos, level])
  }
  bBandPass {
    arg freq = 1200.0, bw = 1.0;
    ^BBandPass.performList(this.rate.rateToSelector, [this, freq, bw])
  }
  bBandStop {
    arg freq = 1200.0, bw = 1.0;
    ^BBandStop.performList(this.rate.rateToSelector, [this, freq, bw])
  }
  bLowPass {
    arg freq = 1200.0, rq = 1.0;
    ^BLowPass.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bpf {
    arg freq = 440.0, rq = 1.0;
    ^BPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bpz2 {
    ^BPZ2.performList(this.rate.rateToSelector, [this])
  }
  brf {
    arg freq = 440.0, rq = 1.0;
    ^BRF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bufWr {
    arg bufnum = 0.0, phase = 0.0, loop = 1.0;
    ^BufWr.performList(this.rate.rateToSelector, [this, bufnum, phase, loop])
  }
  combC {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  combL {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombL.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  combN {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  crossoverDistortion {
    arg amp = 0.5, smooth = 0.5;
    ^CrossoverDistortion.performList(this.rate.rateToSelector, [this, amp, smooth])
  }
  decay {
    arg decayTime = 1.0;
    ^Decay.performList(this.rate.rateToSelector, [this, decayTime])
  }
  decay2 {
    arg attackTime = 1.0e-2, decayTime = 1.0;
    ^Decay2.performList(this.rate.rateToSelector, [this, attackTime, decayTime])
  }
  delayC {
    arg maxdelaytime = 0.2, delaytime = 0.2;
    ^DelayC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime])
  }
  delayN {
    arg maxdelaytime = 0.2, delaytime = 0.2;
    ^DelayN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime])
  }
  demand {
    arg reset = 0.0, demandUGens = 0.0;
    ^Demand.performList(this.rate.rateToSelector, [this, reset, demandUGens])
  }
  detectSilence {
    arg amp = 1.0e-4, time = 0.1, doneAction = 0.0;
    ^DetectSilence.performList(this.rate.rateToSelector, [this, amp, time, doneAction])
  }
  freeVerb {
    arg mix = 0.33, room = 0.5, damp = 0.5;
    ^FreeVerb.performList(this.rate.rateToSelector, [this, mix, room, damp])
  }
  freeVerb2 {
    arg in2 = 0.0, mix = 0.33, room = 0.5, damp = 0.5;
    ^FreeVerb2.performList(this.rate.rateToSelector, [this, in2, mix, room, damp])
  }
  greyholeRaw {
    arg in2 = 0.0, damping = 0.0, delaytime = 2.0, diffusion = 0.5, feedback = 0.9, moddepth = 0.1, modfreq = 2.0, size = 1.0;
    ^GreyholeRaw.performList(this.rate.rateToSelector, [this, in2, damping, delaytime, diffusion, feedback, moddepth, modfreq, size])
  }
  gVerb {
    arg roomsize = 10.0, revtime = 3.0, damping = 0.5, inputbw = 0.5, spread = 15.0, drylevel = 1.0, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300.0;
    ^GVerb.performList(this.rate.rateToSelector, [this, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize])
  }
  hasher {
    ^Hasher.performList(this.rate.rateToSelector, [this])
  }
  hpf {
    arg freq = 440.0;
    ^HPF.performList(this.rate.rateToSelector, [this, freq])
  }
  hpz1 {
    ^HPZ1.performList(this.rate.rateToSelector, [this])
  }
  inRange {
    arg lo = 0.0, hi = 1.0;
    ^InRange.performList(this.rate.rateToSelector, [this, lo, hi])
  }
  integrator {
    arg coef = 1.0;
    ^Integrator.performList(this.rate.rateToSelector, [this, coef])
  }
  klank {
    arg specificationsArrayRef = 0.0, freqscale = 1.0, freqoffset = 0.0, decayscale = 1.0;
    ^Klank.performList(this.rate.rateToSelector, [specificationsArrayRef, this, freqscale, freqoffset, decayscale])
  }
  lpf {
    arg freq = 440.0;
    ^LPF.performList(this.rate.rateToSelector, [this, freq])
  }
  lagUD {
    arg lagTimeU = 0.1, lagTimeD = 0.1;
    ^LagUD.performList(this.rate.rateToSelector, [this, lagTimeU, lagTimeD])
  }
  lag3UD {
    arg lagTimeU = 0.1, lagTimeD = 0.1;
    ^Lag3UD.performList(this.rate.rateToSelector, [this, lagTimeU, lagTimeD])
  }
  latch {
    arg trig = 0.0;
    ^Latch.performList(this.rate.rateToSelector, [this, trig])
  }
  leakDC {
    arg coef = 0.995;
    ^LeakDC.performList(this.rate.rateToSelector, [this, coef])
  }
  limiter {
    arg level = 1.0, dur = 1.0e-2;
    ^Limiter.performList(this.rate.rateToSelector, [this, level, dur])
  }
  linExp {
    arg srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
    ^LinExp.performList(this.rate.rateToSelector, [this, srclo, srchi, dstlo, dsthi])
  }
  linPan2 {
    arg pos = 0.0, level = 1.0;
    ^LinPan2.performList(this.rate.rateToSelector, [this, pos, level])
  }
  linXFade2 {
    arg inB = 0.0, pan = 0.0;
    ^LinXFade2.performList(this.rate.rateToSelector, [this, inB, pan])
  }
  localOut {
    ^LocalOut.performList(this.rate.rateToSelector, [this])
  }
  lpz1 {
    ^LPZ1.performList(this.rate.rateToSelector, [this])
  }
  mantissaMask {
    arg bits = 3.0;
    ^MantissaMask.performList(this.rate.rateToSelector, [this, bits])
  }
  modDif {
    arg y = 0.0, mod = 1.0;
    ^ModDif.performList(this.rate.rateToSelector, [this, y, mod])
  }
  moogFF {
    arg freq = 100.0, gain = 2.0, reset = 0.0;
    ^MoogFF.performList(this.rate.rateToSelector, [this, freq, gain, reset])
  }
  moogLadder {
    arg ffreq = 440.0, res = 0.0;
    ^MoogLadder.performList(this.rate.rateToSelector, [this, ffreq, res])
  }
  mulAdd {
    arg mul = 0.0, add = 0.0;
    ^MulAdd.performList(this.rate.rateToSelector, [this, mul, add])
  }
  normalizer {
    arg level = 1.0, dur = 1.0e-2;
    ^Normalizer.performList(this.rate.rateToSelector, [this, level, dur])
  }
  onePole {
    arg coef = 0.5;
    ^OnePole.performList(this.rate.rateToSelector, [this, coef])
  }
  out {
    arg bus = 0.0;
    ^Out.performList(this.rate.rateToSelector, [bus, this])
  }
  pan2 {
    arg pos = 0.0, level = 1.0;
    ^Pan2.performList(this.rate.rateToSelector, [this, pos, level])
  }
  pitchShift {
    arg windowSize = 0.2, pitchRatio = 1.0, pitchDispersion = 0.0, timeDispersion = 0.0;
    ^PitchShift.performList(this.rate.rateToSelector, [this, windowSize, pitchRatio, pitchDispersion, timeDispersion])
  }
  pluck {
    arg trig = 1.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0, coef = 0.5;
    ^Pluck.performList(this.rate.rateToSelector, [this, trig, maxdelaytime, delaytime, decaytime, coef])
  }
  pulseCount {
    arg reset = 0.0;
    ^PulseCount.performList(this.rate.rateToSelector, [this, reset])
  }
  pulseDivider {
    arg div = 2.0, start = 0.0;
    ^PulseDivider.performList(this.rate.rateToSelector, [this, div, start])
  }
  rhpf {
    arg freq = 440.0, rq = 1.0;
    ^RHPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  rlpf {
    arg freq = 440.0, rq = 1.0;
    ^RLPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  replaceOut {
    arg bus = 0.0;
    ^ReplaceOut.performList(this.rate.rateToSelector, [bus, this])
  }
  resonz {
    arg freq = 440.0, bwr = 1.0;
    ^Resonz.performList(this.rate.rateToSelector, [this, freq, bwr])
  }
  ringz {
    arg freq = 440.0, decaytime = 1.0;
    ^Ringz.performList(this.rate.rateToSelector, [this, freq, decaytime])
  }
  runningMax {
    arg trig = 0.0;
    ^RunningMax.performList(this.rate.rateToSelector, [this, trig])
  }
  rotate2 {
    arg y = 0.0, pos = 0.0;
    ^Rotate2.performList(this.rate.rateToSelector, [this, y, pos])
  }
  setResetFF {
    arg reset = 0.0;
    ^SetResetFF.performList(this.rate.rateToSelector, [this, reset])
  }
  slope {
    ^Slope.performList(this.rate.rateToSelector, [this])
  }
  stepper {
    arg reset = 0.0, min = 0.0, max = 7.0, step = 1.0, resetval = 0.0;
    ^Stepper.performList(this.rate.rateToSelector, [this, reset, min, max, step, resetval])
  }
  sweep {
    arg rate = 1.0;
    ^Sweep.performList(this.rate.rateToSelector, [this, rate])
  }
  tExpRand {
    arg lo = 1.0e-2, hi = 1.0;
    ^TExpRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  timer {
    ^Timer.performList(this.rate.rateToSelector, [this])
  }
  tiRand {
    arg lo = 0.0, hi = 127.0;
    ^TIRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  toggleFF {
    ^ToggleFF.performList(this.rate.rateToSelector, [this])
  }
  tRand {
    arg lo = 0.0, hi = 1.0;
    ^TRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  trig {
    arg dur = 0.1;
    ^Trig.performList(this.rate.rateToSelector, [this, dur])
  }
  trig1 {
    arg dur = 0.1;
    ^Trig1.performList(this.rate.rateToSelector, [this, dur])
  }
  tScramble {
    arg inputs = 0.0;
    ^TScramble.performList(this.rate.rateToSelector, [this, inputs])
  }
  twoPole {
    arg freq = 440.0, radius = 0.8;
    ^TwoPole.performList(this.rate.rateToSelector, [this, freq, radius])
  }
  twoZero {
    arg freq = 440.0, radius = 0.8;
    ^TwoZero.performList(this.rate.rateToSelector, [this, freq, radius])
  }
  xFade2 {
    arg inB = 0.0, pan = 0.0, level = 1.0;
    ^XFade2.performList(this.rate.rateToSelector, [this, inB, pan, level])
  }

}
+ Array {
  allpassC {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  allpassL {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassL.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  allpassN {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^AllpassN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  balance2 {
    arg right = 0.0, pos = 0.0, level = 1.0;
    ^Balance2.performList(this.rate.rateToSelector, [this, right, pos, level])
  }
  bBandPass {
    arg freq = 1200.0, bw = 1.0;
    ^BBandPass.performList(this.rate.rateToSelector, [this, freq, bw])
  }
  bBandStop {
    arg freq = 1200.0, bw = 1.0;
    ^BBandStop.performList(this.rate.rateToSelector, [this, freq, bw])
  }
  bLowPass {
    arg freq = 1200.0, rq = 1.0;
    ^BLowPass.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bpf {
    arg freq = 440.0, rq = 1.0;
    ^BPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bpz2 {
    ^BPZ2.performList(this.rate.rateToSelector, [this])
  }
  brf {
    arg freq = 440.0, rq = 1.0;
    ^BRF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  bufWr {
    arg bufnum = 0.0, phase = 0.0, loop = 1.0;
    ^BufWr.performList(this.rate.rateToSelector, [this, bufnum, phase, loop])
  }
  combC {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  combL {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombL.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  combN {
    arg maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0;
    ^CombN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime, decaytime])
  }
  crossoverDistortion {
    arg amp = 0.5, smooth = 0.5;
    ^CrossoverDistortion.performList(this.rate.rateToSelector, [this, amp, smooth])
  }
  decay {
    arg decayTime = 1.0;
    ^Decay.performList(this.rate.rateToSelector, [this, decayTime])
  }
  decay2 {
    arg attackTime = 1.0e-2, decayTime = 1.0;
    ^Decay2.performList(this.rate.rateToSelector, [this, attackTime, decayTime])
  }
  delayC {
    arg maxdelaytime = 0.2, delaytime = 0.2;
    ^DelayC.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime])
  }
  delayN {
    arg maxdelaytime = 0.2, delaytime = 0.2;
    ^DelayN.performList(this.rate.rateToSelector, [this, maxdelaytime, delaytime])
  }
  demand {
    arg reset = 0.0, demandUGens = 0.0;
    ^Demand.performList(this.rate.rateToSelector, [this, reset, demandUGens])
  }
  detectSilence {
    arg amp = 1.0e-4, time = 0.1, doneAction = 0.0;
    ^DetectSilence.performList(this.rate.rateToSelector, [this, amp, time, doneAction])
  }
  freeVerb {
    arg mix = 0.33, room = 0.5, damp = 0.5;
    ^FreeVerb.performList(this.rate.rateToSelector, [this, mix, room, damp])
  }
  freeVerb2 {
    arg in2 = 0.0, mix = 0.33, room = 0.5, damp = 0.5;
    ^FreeVerb2.performList(this.rate.rateToSelector, [this, in2, mix, room, damp])
  }
  greyholeRaw {
    arg in2 = 0.0, damping = 0.0, delaytime = 2.0, diffusion = 0.5, feedback = 0.9, moddepth = 0.1, modfreq = 2.0, size = 1.0;
    ^GreyholeRaw.performList(this.rate.rateToSelector, [this, in2, damping, delaytime, diffusion, feedback, moddepth, modfreq, size])
  }
  gVerb {
    arg roomsize = 10.0, revtime = 3.0, damping = 0.5, inputbw = 0.5, spread = 15.0, drylevel = 1.0, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300.0;
    ^GVerb.performList(this.rate.rateToSelector, [this, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize])
  }
  hasher {
    ^Hasher.performList(this.rate.rateToSelector, [this])
  }
  hpf {
    arg freq = 440.0;
    ^HPF.performList(this.rate.rateToSelector, [this, freq])
  }
  hpz1 {
    ^HPZ1.performList(this.rate.rateToSelector, [this])
  }
  inRange {
    arg lo = 0.0, hi = 1.0;
    ^InRange.performList(this.rate.rateToSelector, [this, lo, hi])
  }
  integrator {
    arg coef = 1.0;
    ^Integrator.performList(this.rate.rateToSelector, [this, coef])
  }
  klank {
    arg specificationsArrayRef = 0.0, freqscale = 1.0, freqoffset = 0.0, decayscale = 1.0;
    ^Klank.performList(this.rate.rateToSelector, [specificationsArrayRef, this, freqscale, freqoffset, decayscale])
  }
  lpf {
    arg freq = 440.0;
    ^LPF.performList(this.rate.rateToSelector, [this, freq])
  }
  lagUD {
    arg lagTimeU = 0.1, lagTimeD = 0.1;
    ^LagUD.performList(this.rate.rateToSelector, [this, lagTimeU, lagTimeD])
  }
  lag3UD {
    arg lagTimeU = 0.1, lagTimeD = 0.1;
    ^Lag3UD.performList(this.rate.rateToSelector, [this, lagTimeU, lagTimeD])
  }
  latch {
    arg trig = 0.0;
    ^Latch.performList(this.rate.rateToSelector, [this, trig])
  }
  leakDC {
    arg coef = 0.995;
    ^LeakDC.performList(this.rate.rateToSelector, [this, coef])
  }
  limiter {
    arg level = 1.0, dur = 1.0e-2;
    ^Limiter.performList(this.rate.rateToSelector, [this, level, dur])
  }
  linExp {
    arg srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
    ^LinExp.performList(this.rate.rateToSelector, [this, srclo, srchi, dstlo, dsthi])
  }
  linPan2 {
    arg pos = 0.0, level = 1.0;
    ^LinPan2.performList(this.rate.rateToSelector, [this, pos, level])
  }
  linXFade2 {
    arg inB = 0.0, pan = 0.0;
    ^LinXFade2.performList(this.rate.rateToSelector, [this, inB, pan])
  }
  localOut {
    ^LocalOut.performList(this.rate.rateToSelector, [this])
  }
  lpz1 {
    ^LPZ1.performList(this.rate.rateToSelector, [this])
  }
  mantissaMask {
    arg bits = 3.0;
    ^MantissaMask.performList(this.rate.rateToSelector, [this, bits])
  }
  modDif {
    arg y = 0.0, mod = 1.0;
    ^ModDif.performList(this.rate.rateToSelector, [this, y, mod])
  }
  moogFF {
    arg freq = 100.0, gain = 2.0, reset = 0.0;
    ^MoogFF.performList(this.rate.rateToSelector, [this, freq, gain, reset])
  }
  moogLadder {
    arg ffreq = 440.0, res = 0.0;
    ^MoogLadder.performList(this.rate.rateToSelector, [this, ffreq, res])
  }
  mulAdd {
    arg mul = 0.0, add = 0.0;
    ^MulAdd.performList(this.rate.rateToSelector, [this, mul, add])
  }
  normalizer {
    arg level = 1.0, dur = 1.0e-2;
    ^Normalizer.performList(this.rate.rateToSelector, [this, level, dur])
  }
  onePole {
    arg coef = 0.5;
    ^OnePole.performList(this.rate.rateToSelector, [this, coef])
  }
  out {
    arg bus = 0.0;
    ^Out.performList(this.rate.rateToSelector, [bus, this])
  }
  pan2 {
    arg pos = 0.0, level = 1.0;
    ^Pan2.performList(this.rate.rateToSelector, [this, pos, level])
  }
  pitchShift {
    arg windowSize = 0.2, pitchRatio = 1.0, pitchDispersion = 0.0, timeDispersion = 0.0;
    ^PitchShift.performList(this.rate.rateToSelector, [this, windowSize, pitchRatio, pitchDispersion, timeDispersion])
  }
  pluck {
    arg trig = 1.0, maxdelaytime = 0.2, delaytime = 0.2, decaytime = 1.0, coef = 0.5;
    ^Pluck.performList(this.rate.rateToSelector, [this, trig, maxdelaytime, delaytime, decaytime, coef])
  }
  pulseCount {
    arg reset = 0.0;
    ^PulseCount.performList(this.rate.rateToSelector, [this, reset])
  }
  pulseDivider {
    arg div = 2.0, start = 0.0;
    ^PulseDivider.performList(this.rate.rateToSelector, [this, div, start])
  }
  rhpf {
    arg freq = 440.0, rq = 1.0;
    ^RHPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  rlpf {
    arg freq = 440.0, rq = 1.0;
    ^RLPF.performList(this.rate.rateToSelector, [this, freq, rq])
  }
  replaceOut {
    arg bus = 0.0;
    ^ReplaceOut.performList(this.rate.rateToSelector, [bus, this])
  }
  resonz {
    arg freq = 440.0, bwr = 1.0;
    ^Resonz.performList(this.rate.rateToSelector, [this, freq, bwr])
  }
  ringz {
    arg freq = 440.0, decaytime = 1.0;
    ^Ringz.performList(this.rate.rateToSelector, [this, freq, decaytime])
  }
  runningMax {
    arg trig = 0.0;
    ^RunningMax.performList(this.rate.rateToSelector, [this, trig])
  }
  rotate2 {
    arg y = 0.0, pos = 0.0;
    ^Rotate2.performList(this.rate.rateToSelector, [this, y, pos])
  }
  setResetFF {
    arg reset = 0.0;
    ^SetResetFF.performList(this.rate.rateToSelector, [this, reset])
  }
  slope {
    ^Slope.performList(this.rate.rateToSelector, [this])
  }
  stepper {
    arg reset = 0.0, min = 0.0, max = 7.0, step = 1.0, resetval = 0.0;
    ^Stepper.performList(this.rate.rateToSelector, [this, reset, min, max, step, resetval])
  }
  sweep {
    arg rate = 1.0;
    ^Sweep.performList(this.rate.rateToSelector, [this, rate])
  }
  tExpRand {
    arg lo = 1.0e-2, hi = 1.0;
    ^TExpRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  timer {
    ^Timer.performList(this.rate.rateToSelector, [this])
  }
  tiRand {
    arg lo = 0.0, hi = 127.0;
    ^TIRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  toggleFF {
    ^ToggleFF.performList(this.rate.rateToSelector, [this])
  }
  tRand {
    arg lo = 0.0, hi = 1.0;
    ^TRand.performList(this.rate.rateToSelector, [lo, hi, this])
  }
  trig {
    arg dur = 0.1;
    ^Trig.performList(this.rate.rateToSelector, [this, dur])
  }
  trig1 {
    arg dur = 0.1;
    ^Trig1.performList(this.rate.rateToSelector, [this, dur])
  }
  tScramble {
    arg inputs = 0.0;
    ^TScramble.performList(this.rate.rateToSelector, [this, inputs])
  }
  twoPole {
    arg freq = 440.0, radius = 0.8;
    ^TwoPole.performList(this.rate.rateToSelector, [this, freq, radius])
  }
  twoZero {
    arg freq = 440.0, radius = 0.8;
    ^TwoZero.performList(this.rate.rateToSelector, [this, freq, radius])
  }
  xFade2 {
    arg inB = 0.0, pan = 0.0, level = 1.0;
    ^XFade2.performList(this.rate.rateToSelector, [this, inB, pan, level])
  }

}
