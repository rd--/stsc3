+ UGen {
  apf {
    arg freq = 440.0,radius = 0.8;
    ^APF.multiNew(this.rate,this,freq,radius)
  }
  allpassC {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^AllpassC.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  allpassL {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^AllpassL.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  allpassN {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^AllpassN.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  bAllPass {
    arg freq = 1200.0,rq = 1.0;
    ^BAllPass.multiNew(this.rate,this,freq,rq)
  }
  bBandPass {
    arg freq = 1200.0,bw = 1.0;
    ^BBandPass.multiNew(this.rate,this,freq,bw)
  }
  bBandStop {
    arg freq = 1200.0,bw = 1.0;
    ^BBandStop.multiNew(this.rate,this,freq,bw)
  }
  bHiPass {
    arg freq = 1200.0,rq = 1.0;
    ^BHiPass.multiNew(this.rate,this,freq,rq)
  }
  bHiShelf {
    arg freq = 1200.0,rs = 1.0,db = 0.0;
    ^BHiShelf.multiNew(this.rate,this,freq,rs,db)
  }
  bLowPass {
    arg freq = 1200.0,rq = 1.0;
    ^BLowPass.multiNew(this.rate,this,freq,rq)
  }
  bLowShelf {
    arg freq = 1200.0,rs = 1.0,db = 0.0;
    ^BLowShelf.multiNew(this.rate,this,freq,rs,db)
  }
  bpf {
    arg freq = 440.0,rq = 1.0;
    ^BPF.multiNew(this.rate,this,freq,rq)
  }
  bpz2 {
    ^BPZ2.multiNew(this.rate,this)
  }
  bPeakEQ {
    arg freq = 1200.0,rq = 1.0,db = 0.0;
    ^BPeakEQ.multiNew(this.rate,this,freq,rq,db)
  }
  brf {
    arg freq = 440.0,rq = 1.0;
    ^BRF.multiNew(this.rate,this,freq,rq)
  }
  brz2 {
    ^BRZ2.multiNew(this.rate,this)
  }
  bufAllpassC {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufAllpassC.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufAllpassL {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufAllpassL.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufAllpassN {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufAllpassN.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufCombC {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufCombC.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufCombL {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufCombL.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufCombN {
    arg buf = 0.0,delaytime = 0.2,decaytime = 1.0;
    ^BufCombN.multiNew(this.rate,buf,this,delaytime,decaytime)
  }
  bufDelayC {
    arg buf = 0.0,delaytime = 0.2;
    ^BufDelayC.multiNew(this.rate,buf,this,delaytime)
  }
  bufDelayL {
    arg buf = 0.0,delaytime = 0.2;
    ^BufDelayL.multiNew(this.rate,buf,this,delaytime)
  }
  bufDelayN {
    arg buf = 0.0,delaytime = 0.2;
    ^BufDelayN.multiNew(this.rate,buf,this,delaytime)
  }
  bufWr {
    arg bufnum = 0.0,phase = 0.0,loop = 1.0;
    ^BufWr.multiNew(this.rate,bufnum,phase,loop,this)
  }
  coinGate {
    arg prob = 0.0;
    ^CoinGate.multiNew(this.rate,prob,this)
  }
  combC {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^CombC.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  combL {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^CombL.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  combN {
    arg maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0;
    ^CombN.multiNew(this.rate,this,maxdelaytime,delaytime,decaytime)
  }
  compander {
    arg control = 0.0,thresh = 0.5,slopeBelow = 1.0,slopeAbove = 1.0,clampTime = 1.0e-2,relaxTime = 0.1;
    ^Compander.multiNew(this.rate,this,control,thresh,slopeBelow,slopeAbove,clampTime,relaxTime)
  }
  decay {
    arg decayTime = 1.0;
    ^Decay.multiNew(this.rate,this,decayTime)
  }
  decay2 {
    arg attackTime = 1.0e-2,decayTime = 1.0;
    ^Decay2.multiNew(this.rate,this,attackTime,decayTime)
  }
  delTapRd {
    arg buffer = 0.0,delTime = 0.0,interp = 1.0;
    ^DelTapRd.multiNew(this.rate,buffer,this,delTime,interp)
  }
  delTapWr {
    arg buffer = 0.0;
    ^DelTapWr.multiNew(this.rate,buffer,this)
  }
  delay1 {
    ^Delay1.multiNew(this.rate,this)
  }
  delay2 {
    ^Delay2.multiNew(this.rate,this)
  }
  delayC {
    arg maxdelaytime = 0.2,delaytime = 0.2;
    ^DelayC.multiNew(this.rate,this,maxdelaytime,delaytime)
  }
  delayL {
    arg maxdelaytime = 0.2,delaytime = 0.2;
    ^DelayL.multiNew(this.rate,this,maxdelaytime,delaytime)
  }
  delayN {
    arg maxdelaytime = 0.2,delaytime = 0.2;
    ^DelayN.multiNew(this.rate,this,maxdelaytime,delaytime)
  }
  demand {
    arg reset = 0.0,demandUGens = 0.0;
    ^Demand.multiNew(this.rate,this,reset,demandUGens)
  }
  detectIndex {
    arg bufnum = 0.0;
    ^DetectIndex.multiNew(this.rate,bufnum,this)
  }
  detectSilence {
    arg amp = 1.0e-4,time = 0.1,doneAction = 0.0;
    ^DetectSilence.multiNew(this.rate,this,amp,time,doneAction)
  }
  fos {
    arg a0 = 0.0,a1 = 0.0,b1 = 0.0;
    ^FOS.multiNew(this.rate,this,a0,a1,b1)
  }
  formlet {
    arg freq = 440.0,attacktime = 1.0,decaytime = 1.0;
    ^Formlet.multiNew(this.rate,this,freq,attacktime,decaytime)
  }
  free {
    arg id = 0.0;
    ^Free.multiNew(this.rate,this,id)
  }
  freeVerb {
    arg mix = 0.33,room = 0.5,damp = 0.5;
    ^FreeVerb.multiNew(this.rate,this,mix,room,damp)
  }
  freeVerb2 {
    arg in2 = 0.0,mix = 0.33,room = 0.5,damp = 0.5;
    ^FreeVerb2.multiNew(this.rate,this,in2,mix,room,damp)
  }
  gVerb {
    arg roomsize = 10.0,revtime = 3.0,damping = 0.5,inputbw = 0.5,spread = 15.0,drylevel = 1.0,earlyreflevel = 0.7,taillevel = 0.5,maxroomsize = 300.0;
    ^GVerb.multiNew(this.rate,this,roomsize,revtime,damping,inputbw,spread,drylevel,earlyreflevel,taillevel,maxroomsize)
  }
  gate {
    arg trig = 0.0;
    ^Gate.multiNew(this.rate,this,trig)
  }
  hpf {
    arg freq = 440.0;
    ^HPF.multiNew(this.rate,this,freq)
  }
  hpz1 {
    ^HPZ1.multiNew(this.rate,this)
  }
  hpz2 {
    ^HPZ2.multiNew(this.rate,this)
  }
  hasher {
    ^Hasher.multiNew(this.rate,this)
  }
  hilbert {
    ^Hilbert.multiNew(this.rate,this)
  }
  inRange {
    arg lo = 0.0,hi = 1.0;
    ^InRange.multiNew(this.rate,this,lo,hi)
  }
  index {
    arg bufnum = 0.0;
    ^Index.multiNew(this.rate,bufnum,this)
  }
  indexInBetween {
    arg bufnum = 0.0;
    ^IndexInBetween.multiNew(this.rate,bufnum,this)
  }
  indexL {
    arg bufnum = 0.0;
    ^IndexL.multiNew(this.rate,bufnum,this)
  }
  integrator {
    arg coef = 1.0;
    ^Integrator.multiNew(this.rate,this,coef)
  }
  klank {
    arg freqscale = 1.0,freqoffset = 0.0,decayscale = 1.0,specificationsArrayRef = 0.0;
    ^Klank.multiNew(this.rate,this,freqscale,freqoffset,decayscale,specificationsArrayRef)
  }
  lpf {
    arg freq = 440.0;
    ^LPF.multiNew(this.rate,this,freq)
  }
  lpz1 {
    ^LPZ1.multiNew(this.rate,this)
  }
  lpz2 {
    ^LPZ2.multiNew(this.rate,this)
  }
  lag2UD {
    arg lagTimeU = 0.1,lagTimeD = 0.1;
    ^Lag2UD.multiNew(this.rate,this,lagTimeU,lagTimeD)
  }
  lag3UD {
    arg lagTimeU = 0.1,lagTimeD = 0.1;
    ^Lag3UD.multiNew(this.rate,this,lagTimeU,lagTimeD)
  }
  lagUD {
    arg lagTimeU = 0.1,lagTimeD = 0.1;
    ^LagUD.multiNew(this.rate,this,lagTimeU,lagTimeD)
  }
  lastValue {
    arg diff = 1.0e-2;
    ^LastValue.multiNew(this.rate,this,diff)
  }
  leakDC {
    arg coef = 0.995;
    ^LeakDC.multiNew(this.rate,this,coef)
  }
  limiter {
    arg level = 1.0,dur = 1.0e-2;
    ^Limiter.multiNew(this.rate,this,level,dur)
  }
  linExp {
    arg srclo = 0.0,srchi = 1.0,dstlo = 1.0,dsthi = 2.0;
    ^LinExp.multiNew(this.rate,this,srclo,srchi,dstlo,dsthi)
  }
  linPan2 {
    arg pos = 0.0,level = 1.0;
    ^LinPan2.multiNew(this.rate,this,pos,level)
  }
  localOut {
    ^LocalOut.multiNew(this.rate,this)
  }
  mantissaMask {
    arg bits = 3.0;
    ^MantissaMask.multiNew(this.rate,this,bits)
  }
  median {
    arg length = 3.0;
    ^Median.multiNew(this.rate,length,this)
  }
  midEQ {
    arg freq = 440.0,rq = 1.0,db = 0.0;
    ^MidEQ.multiNew(this.rate,this,freq,rq,db)
  }
  modDif {
    arg y = 0.0,mod = 1.0;
    ^ModDif.multiNew(this.rate,this,y,mod)
  }
  moogFF {
    arg freq = 100.0,gain = 2.0,reset = 0.0;
    ^MoogFF.multiNew(this.rate,this,freq,gain,reset)
  }
  normalizer {
    arg level = 1.0,dur = 1.0e-2;
    ^Normalizer.multiNew(this.rate,this,level,dur)
  }
  offsetOut {
    arg bus = 0.0;
    ^OffsetOut.multiNew(this.rate,bus,this)
  }
  onePole {
    arg coef = 0.5;
    ^OnePole.multiNew(this.rate,this,coef)
  }
  oneZero {
    arg coef = 0.5;
    ^OneZero.multiNew(this.rate,this,coef)
  }
  out {
    arg bus = 0.0;
    ^Out.multiNew(this.rate,bus,this)
  }
  pan2 {
    arg pos = 0.0,level = 1.0;
    ^Pan2.multiNew(this.rate,this,pos,level)
  }
  panAz {
    arg pos = 0.0,level = 1.0,width = 2.0,orientation = 0.5;
    ^PanAz.multiNew(this.rate,this,pos,level,width,orientation)
  }
  panB2 {
    arg azimuth = 0.0,gain = 1.0;
    ^PanB2.multiNew(this.rate,this,azimuth,gain)
  }
  peak {
    arg trig = 0.0;
    ^Peak.multiNew(this.rate,this,trig)
  }
  peakFollower {
    arg decay = 0.999;
    ^PeakFollower.multiNew(this.rate,this,decay)
  }
  pitchShift {
    arg windowSize = 0.2,pitchRatio = 1.0,pitchDispersion = 0.0,timeDispersion = 0.0;
    ^PitchShift.multiNew(this.rate,this,windowSize,pitchRatio,pitchDispersion,timeDispersion)
  }
  pluck {
    arg trig = 1.0,maxdelaytime = 0.2,delaytime = 0.2,decaytime = 1.0,coef = 0.5;
    ^Pluck.multiNew(this.rate,this,trig,maxdelaytime,delaytime,decaytime,coef)
  }
  pulseCount {
    arg reset = 0.0;
    ^PulseCount.multiNew(this.rate,this,reset)
  }
  pulseDivider {
    arg div = 2.0,start = 0.0;
    ^PulseDivider.multiNew(this.rate,this,div,start)
  }
  rhpf {
    arg freq = 440.0,rq = 1.0;
    ^RHPF.multiNew(this.rate,this,freq,rq)
  }
  rlpf {
    arg freq = 440.0,rq = 1.0;
    ^RLPF.multiNew(this.rate,this,freq,rq)
  }
  ramp {
    arg lagTime = 0.1;
    ^Ramp.multiNew(this.rate,this,lagTime)
  }
  replaceOut {
    arg bus = 0.0;
    ^ReplaceOut.multiNew(this.rate,bus,this)
  }
  resonz {
    arg freq = 440.0,bwr = 1.0;
    ^Resonz.multiNew(this.rate,this,freq,bwr)
  }
  ringz {
    arg freq = 440.0,decaytime = 1.0;
    ^Ringz.multiNew(this.rate,this,freq,decaytime)
  }
  runningMax {
    arg trig = 0.0;
    ^RunningMax.multiNew(this.rate,this,trig)
  }
  runningMin {
    arg trig = 0.0;
    ^RunningMin.multiNew(this.rate,this,trig)
  }
  runningSum {
    arg numsamp = 40.0;
    ^RunningSum.multiNew(this.rate,this,numsamp)
  }
  sos {
    arg a0 = 0.0,a1 = 0.0,a2 = 0.0,b1 = 0.0,b2 = 0.0;
    ^SOS.multiNew(this.rate,this,a0,a1,a2,b1,b2)
  }
  schmidt {
    arg lo = 0.0,hi = 1.0;
    ^Schmidt.multiNew(this.rate,this,lo,hi)
  }
  sendTrig {
    arg id = 0.0,value = 0.0;
    ^SendTrig.multiNew(this.rate,this,id,value)
  }
  shaper {
    arg bufnum = 0.0;
    ^Shaper.multiNew(this.rate,bufnum,this)
  }
  slope {
    ^Slope.multiNew(this.rate,this)
  }
  stepper {
    arg reset = 0.0,min = 0.0,max = 7.0,step = 1.0,resetval = 0.0;
    ^Stepper.multiNew(this.rate,this,reset,min,max,step,resetval)
  }
  sweep {
    arg rate = 1.0;
    ^Sweep.multiNew(this.rate,this,rate)
  }
  tDelay {
    arg dur = 0.1;
    ^TDelay.multiNew(this.rate,this,dur)
  }
  tExpRand {
    arg lo = 1.0e-2,hi = 1.0;
    ^TExpRand.multiNew(this.rate,lo,hi,this)
  }
  tiRand {
    arg lo = 0.0,hi = 127.0;
    ^TIRand.multiNew(this.rate,lo,hi,this)
  }
  tRand {
    arg lo = 0.0,hi = 1.0;
    ^TRand.multiNew(this.rate,lo,hi,this)
  }
  tWindex {
    arg normalize = 0.0,array = 0.0;
    ^TWindex.multiNew(this.rate,this,normalize,array)
  }
  timer {
    ^Timer.multiNew(this.rate,this)
  }
  toggleFF {
    ^ToggleFF.multiNew(this.rate,this)
  }
  trig {
    arg dur = 0.1;
    ^Trig.multiNew(this.rate,this,dur)
  }
  trig1 {
    arg dur = 0.1;
    ^Trig1.multiNew(this.rate,this,dur)
  }
  twoPole {
    arg freq = 440.0,radius = 0.8;
    ^TwoPole.multiNew(this.rate,this,freq,radius)
  }
  twoZero {
    arg freq = 440.0,radius = 0.8;
    ^TwoZero.multiNew(this.rate,this,freq,radius)
  }
  varLag {
    arg time = 0.1,curvature = 0.0,warp = 5.0,start = 0.0;
    ^VarLag.multiNew(this.rate,this,time,curvature,warp,start)
  }
  wrapIndex {
    arg bufnum = 0.0;
    ^WrapIndex.multiNew(this.rate,bufnum,this)
  }
  xOut {
    arg bus = 0.0,xfade = 0.0;
    ^XOut.multiNew(this.rate,bus,xfade,this)
  }
  zeroCrossing {
    ^ZeroCrossing.multiNew(this.rate,this)
  }

}
