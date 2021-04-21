UGen subclass: #AllpassN
 instanceVariableNames: 'in maxdelaytime delaytime decaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

AllpassN comment: 'Schroeder allpass delay line with no interpolation.'!

!AllpassN methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
!

!AllpassN methodsFor: 'initialization'!
 initialize
  ugenName := #AllpassN.
  paramNames := #(#in #maxdelaytime #delaytime #decaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!

!AllpassN class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!AllpassN class methodsFor: 'instance creation'!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^u!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul)!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul + add)!
!

UGen subclass: #AmpComp
 instanceVariableNames: 'freq root exp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

AmpComp comment: 'Basic psychoacoustic amplitude compensation.'!

!AmpComp methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 root ^root!
 root: x root := x!
 exp ^exp!
 exp: x exp := x!
!

!AmpComp methodsFor: 'initialization'!
 initialize
  ugenName := #AmpComp.
  paramNames := #(#freq #root #exp).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 0.0. root := 0.0. exp := 0.3333!
!

!AmpComp class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!AmpComp class methodsFor: 'instance creation'!
 freq: freq root: root exp: exp
  |u|
  u := self new.
  u freq: freq.
  u root: root.
  u exp: exp.
  ^u!
 freq: freq root: root exp: exp mul: mul
  |u|
  u := self new.
  u freq: freq.
  u root: root.
  u exp: exp.
  ^(u * mul)!
 freq: freq root: root exp: exp mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u root: root.
  u exp: exp.
  ^(u * mul + add)!
!

UGen subclass: #Amplitude
 instanceVariableNames: 'in attackTime releaseTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Amplitude comment: 'Amplitude follower'!

!Amplitude methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 attackTime ^attackTime!
 attackTime: x attackTime := x!
 releaseTime ^releaseTime!
 releaseTime: x releaseTime := x!
!

!Amplitude methodsFor: 'initialization'!
 initialize
  ugenName := #Amplitude.
  paramNames := #(#in #attackTime #releaseTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. attackTime := 0.01. releaseTime := 0.01!
!

!Amplitude class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Amplitude class methodsFor: 'instance creation'!
 in: in attackTime: attackTime releaseTime: releaseTime
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u releaseTime: releaseTime.
  ^u!
 in: in attackTime: attackTime releaseTime: releaseTime mul: mul
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u releaseTime: releaseTime.
  ^(u * mul)!
 in: in attackTime: attackTime releaseTime: releaseTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u releaseTime: releaseTime.
  ^(u * mul + add)!
!

UGen subclass: #BPF
 instanceVariableNames: 'in freq rq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

BPF comment: '2nd order Butterworth bandpass filter.'!

!BPF methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
 rq ^rq!
 rq: x rq := x!
!

!BPF methodsFor: 'initialization'!
 initialize
  ugenName := #BPF.
  paramNames := #(#in #freq #rq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0. rq := 1.0!
!

!BPF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!BPF class methodsFor: 'instance creation'!
 in: in freq: freq rq: rq
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^u!
 in: in freq: freq rq: rq mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul)!
 in: in freq: freq rq: rq mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul + add)!
!

UGen subclass: #BPZ2
 instanceVariableNames: 'in'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

BPZ2 comment: 'Two zero fixed midpass.'!

!BPZ2 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
!

!BPZ2 methodsFor: 'initialization'!
 initialize
  ugenName := #BPZ2.
  paramNames := #(#in).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0!
!

!BPZ2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!BPZ2 class methodsFor: 'instance creation'!
 in: in
  |u|
  u := self new.
  u in: in.
  ^u!
 in: in mul: mul
  |u|
  u := self new.
  u in: in.
  ^(u * mul)!
 in: in mul: mul add: add
  |u|
  u := self new.
  u in: in.
  ^(u * mul + add)!
!

UGen subclass: #BrownNoise
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

BrownNoise comment: 'Brown Noise.'!


!BrownNoise methodsFor: 'initialization'!
 initialize
  ugenName := #BrownNoise.
  paramNames := #().
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  !
!

!BrownNoise class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!BrownNoise class methodsFor: 'instance creation'!
  mul: mul
  |u|
  u := self new.
  
  ^(u * mul)!
  mul: mul add: add
  |u|
  u := self new.
  
  ^(u * mul + add)!
!

UGen subclass: #CombC
 instanceVariableNames: 'in maxdelaytime delaytime decaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

CombC comment: 'Comb delay line with cubic interpolation.'!

!CombC methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
!

!CombC methodsFor: 'initialization'!
 initialize
  ugenName := #CombC.
  paramNames := #(#in #maxdelaytime #delaytime #decaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!

!CombC class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!CombC class methodsFor: 'instance creation'!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^u!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul)!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul + add)!
!

UGen subclass: #CombL
 instanceVariableNames: 'in maxdelaytime delaytime decaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

CombL comment: 'Comb delay line with linear interpolation.'!

!CombL methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
!

!CombL methodsFor: 'initialization'!
 initialize
  ugenName := #CombL.
  paramNames := #(#in #maxdelaytime #delaytime #decaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!

!CombL class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!CombL class methodsFor: 'instance creation'!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^u!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul)!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul + add)!
!

UGen subclass: #CombN
 instanceVariableNames: 'in maxdelaytime delaytime decaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

CombN comment: 'Comb delay line with no interpolation.'!

!CombN methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
!

!CombN methodsFor: 'initialization'!
 initialize
  ugenName := #CombN.
  paramNames := #(#in #maxdelaytime #delaytime #decaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0!
!

!CombN class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!CombN class methodsFor: 'instance creation'!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^u!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul)!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  ^(u * mul + add)!
!

UGen subclass: #Crackle
 instanceVariableNames: 'chaosParam'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Crackle comment: 'Chaotic noise function.'!

!Crackle methodsFor: 'accessing'!
 chaosParam ^chaosParam!
 chaosParam: x chaosParam := x!
!

!Crackle methodsFor: 'initialization'!
 initialize
  ugenName := #Crackle.
  paramNames := #(#chaosParam).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  chaosParam := 1.5!
!

!Crackle class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Crackle class methodsFor: 'instance creation'!
 chaosParam: chaosParam
  |u|
  u := self new.
  u chaosParam: chaosParam.
  ^u!
 chaosParam: chaosParam mul: mul
  |u|
  u := self new.
  u chaosParam: chaosParam.
  ^(u * mul)!
 chaosParam: chaosParam mul: mul add: add
  |u|
  u := self new.
  u chaosParam: chaosParam.
  ^(u * mul + add)!
!

UGen subclass: #Decay
 instanceVariableNames: 'in decayTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Decay comment: 'Exponential decay'!

!Decay methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 decayTime ^decayTime!
 decayTime: x decayTime := x!
!

!Decay methodsFor: 'initialization'!
 initialize
  ugenName := #Decay.
  paramNames := #(#in #decayTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. decayTime := 1.0!
!

!Decay class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Decay class methodsFor: 'instance creation'!
 in: in decayTime: decayTime
  |u|
  u := self new.
  u in: in.
  u decayTime: decayTime.
  ^u!
 in: in decayTime: decayTime mul: mul
  |u|
  u := self new.
  u in: in.
  u decayTime: decayTime.
  ^(u * mul)!
 in: in decayTime: decayTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u decayTime: decayTime.
  ^(u * mul + add)!
!

UGen subclass: #Decay2
 instanceVariableNames: 'in attackTime decayTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Decay2 comment: 'Exponential decay'!

!Decay2 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 attackTime ^attackTime!
 attackTime: x attackTime := x!
 decayTime ^decayTime!
 decayTime: x decayTime := x!
!

!Decay2 methodsFor: 'initialization'!
 initialize
  ugenName := #Decay2.
  paramNames := #(#in #attackTime #decayTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. attackTime := 0.01. decayTime := 1.0!
!

!Decay2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Decay2 class methodsFor: 'instance creation'!
 in: in attackTime: attackTime decayTime: decayTime
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u decayTime: decayTime.
  ^u!
 in: in attackTime: attackTime decayTime: decayTime mul: mul
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u decayTime: decayTime.
  ^(u * mul)!
 in: in attackTime: attackTime decayTime: decayTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u attackTime: attackTime.
  u decayTime: decayTime.
  ^(u * mul + add)!
!

UGen subclass: #DegreeToKey
 instanceVariableNames: 'bufnum in octave'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

DegreeToKey comment: 'Convert signal to modal pitch.'!

!DegreeToKey methodsFor: 'accessing'!
 bufnum ^bufnum!
 bufnum: x bufnum := x!
 in ^in!
 in: x in := x!
 octave ^octave!
 octave: x octave := x!
!

!DegreeToKey methodsFor: 'initialization'!
 initialize
  ugenName := #DegreeToKey.
  paramNames := #(#bufnum #in #octave).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  bufnum := 0.0. in := 0.0. octave := 12.0!
!

!DegreeToKey class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!DegreeToKey class methodsFor: 'instance creation'!
 bufnum: bufnum in: in octave: octave
  |u|
  u := self new.
  u bufnum: bufnum.
  u in: in.
  u octave: octave.
  ^u!
 bufnum: bufnum in: in octave: octave mul: mul
  |u|
  u := self new.
  u bufnum: bufnum.
  u in: in.
  u octave: octave.
  ^(u * mul)!
 bufnum: bufnum in: in octave: octave mul: mul add: add
  |u|
  u := self new.
  u bufnum: bufnum.
  u in: in.
  u octave: octave.
  ^(u * mul + add)!
!

UGen subclass: #DelayN
 instanceVariableNames: 'in maxdelaytime delaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

DelayN comment: 'Simple delay line with no interpolation.'!

!DelayN methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
!

!DelayN methodsFor: 'initialization'!
 initialize
  ugenName := #DelayN.
  paramNames := #(#in #maxdelaytime #delaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. maxdelaytime := 0.2. delaytime := 0.2!
!

!DelayN class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!DelayN class methodsFor: 'instance creation'!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  ^u!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  ^(u * mul)!
 in: in maxdelaytime: maxdelaytime delaytime: delaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  ^(u * mul + add)!
!

UGen subclass: #Demand
 instanceVariableNames: 'trig reset demandUGens'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Demand comment: 'Demand results from demand rate UGens.'!

!Demand methodsFor: 'accessing'!
 trig ^trig!
 trig: x trig := x!
 reset ^reset!
 reset: x reset := x!
 demandUGens ^demandUGens!
 demandUGens: x demandUGens := x!
!

!Demand methodsFor: 'initialization'!
 initialize
  ugenName := #Demand.
  paramNames := #(#trig #reset).
  mceNames := #(#demandUGens).
  ugenRate := #ar.
  numChan := nil.
  specialIndex := 0.
  trig := 0.0. reset := 0.0!
!

!Demand class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Demand class methodsFor: 'instance creation'!
 trig: trig reset: reset demandUGens: demandUGens
  |u|
  u := self new.
  u numChan: #mceInput. 
  u trig: trig.
  u reset: reset.
  u demandUGens: demandUGens.
  ^u!
 trig: trig reset: reset demandUGens: demandUGens mul: mul
  |u|
  u := self new.
  u numChan: #mceInput. 
  u trig: trig.
  u reset: reset.
  u demandUGens: demandUGens.
  ^(u * mul)!
 trig: trig reset: reset demandUGens: demandUGens mul: mul add: add
  |u|
  u := self new.
  u numChan: #mceInput. 
  u trig: trig.
  u reset: reset.
  u demandUGens: demandUGens.
  ^(u * mul + add)!
!

UGen subclass: #Drand
 instanceVariableNames: 'repeats list'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Drand comment: 'Demand rate random sequence generator.'!

!Drand methodsFor: 'accessing'!
 repeats ^repeats!
 repeats: x repeats := x!
 list ^list!
 list: x list := x!
!

!Drand methodsFor: 'initialization'!
 initialize
  ugenName := #Drand.
  paramNames := #(#repeats).
  mceNames := #(#list).
  ugenRate := #dr.
  numChan := 1.
  specialIndex := 0.
  repeats := 1.0!
!

!Drand class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Drand class methodsFor: 'instance creation'!
 repeats: repeats list: list
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^u!
 repeats: repeats list: list mul: mul
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul)!
 repeats: repeats list: list mul: mul add: add
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul + add)!
!

UGen subclass: #Dseq
 instanceVariableNames: 'repeats list'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Dseq comment: 'Demand rate sequence generator.'!

!Dseq methodsFor: 'accessing'!
 repeats ^repeats!
 repeats: x repeats := x!
 list ^list!
 list: x list := x!
!

!Dseq methodsFor: 'initialization'!
 initialize
  ugenName := #Dseq.
  paramNames := #(#repeats).
  mceNames := #(#list).
  ugenRate := #dr.
  numChan := 1.
  specialIndex := 0.
  repeats := 1.0!
!

!Dseq class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Dseq class methodsFor: 'instance creation'!
 repeats: repeats list: list
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^u!
 repeats: repeats list: list mul: mul
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul)!
 repeats: repeats list: list mul: mul add: add
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul + add)!
!

UGen subclass: #Dshuf
 instanceVariableNames: 'repeats list'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Dshuf comment: 'Demand rate random sequence generator'!

!Dshuf methodsFor: 'accessing'!
 repeats ^repeats!
 repeats: x repeats := x!
 list ^list!
 list: x list := x!
!

!Dshuf methodsFor: 'initialization'!
 initialize
  ugenName := #Dshuf.
  paramNames := #(#repeats).
  mceNames := #(#list).
  ugenRate := #dr.
  numChan := 1.
  specialIndex := 0.
  repeats := 1.0!
!

!Dshuf class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Dshuf class methodsFor: 'instance creation'!
 repeats: repeats list: list
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^u!
 repeats: repeats list: list mul: mul
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul)!
 repeats: repeats list: list mul: mul add: add
  |u|
  u := self new.
  u repeats: repeats.
  u list: list.
  ^(u * mul + add)!
!

UGen subclass: #Dust
 instanceVariableNames: 'density'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Dust comment: 'Random impulses.'!

!Dust methodsFor: 'accessing'!
 density ^density!
 density: x density := x!
!

!Dust methodsFor: 'initialization'!
 initialize
  ugenName := #Dust.
  paramNames := #(#density).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  density := 0.0!
!

!Dust class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Dust class methodsFor: 'instance creation'!
 density: density
  |u|
  u := self new.
  u density: density.
  ^u!
 density: density mul: mul
  |u|
  u := self new.
  u density: density.
  ^(u * mul)!
 density: density mul: mul add: add
  |u|
  u := self new.
  u density: density.
  ^(u * mul + add)!
!

UGen subclass: #Duty
 instanceVariableNames: 'dur reset doneAction level'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Duty comment: 'Demand results from demand rate UGens.'!

!Duty methodsFor: 'accessing'!
 dur ^dur!
 dur: x dur := x!
 reset ^reset!
 reset: x reset := x!
 doneAction ^doneAction!
 doneAction: x doneAction := x!
 level ^level!
 level: x level := x!
!

!Duty methodsFor: 'initialization'!
 initialize
  ugenName := #Duty.
  paramNames := #(#dur #reset #doneAction #level).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  dur := 1.0. reset := 0.0. doneAction := 0.0. level := 1.0!
!

!Duty class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Duty class methodsFor: 'instance creation'!
 dur: dur reset: reset doneAction: doneAction level: level
  |u|
  u := self new.
  u dur: dur.
  u reset: reset.
  u doneAction: doneAction.
  u level: level.
  ^u!
 dur: dur reset: reset doneAction: doneAction level: level mul: mul
  |u|
  u := self new.
  u dur: dur.
  u reset: reset.
  u doneAction: doneAction.
  u level: level.
  ^(u * mul)!
 dur: dur reset: reset doneAction: doneAction level: level mul: mul add: add
  |u|
  u := self new.
  u dur: dur.
  u reset: reset.
  u doneAction: doneAction.
  u level: level.
  ^(u * mul + add)!
!

UGen subclass: #EnvGen
 instanceVariableNames: 'gate levelScale levelBias timeScale doneAction envelope'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

EnvGen comment: 'Envelope generator'!

!EnvGen methodsFor: 'accessing'!
 gate ^gate!
 gate: x gate := x!
 levelScale ^levelScale!
 levelScale: x levelScale := x!
 levelBias ^levelBias!
 levelBias: x levelBias := x!
 timeScale ^timeScale!
 timeScale: x timeScale := x!
 doneAction ^doneAction!
 doneAction: x doneAction := x!
 envelope ^envelope!
 envelope: x envelope := x!
!

!EnvGen methodsFor: 'initialization'!
 initialize
  ugenName := #EnvGen.
  paramNames := #(#gate #levelScale #levelBias #timeScale #doneAction).
  mceNames := #(#envelope).
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  gate := 1.0. levelScale := 1.0. levelBias := 0.0. timeScale := 1.0. doneAction := 0.0!
!

!EnvGen class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!EnvGen class methodsFor: 'instance creation'!
 gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope
  |u|
  u := self new.
  u gate: gate.
  u levelScale: levelScale.
  u levelBias: levelBias.
  u timeScale: timeScale.
  u doneAction: doneAction.
  u envelope: envelope.
  ^u!
 gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul
  |u|
  u := self new.
  u gate: gate.
  u levelScale: levelScale.
  u levelBias: levelBias.
  u timeScale: timeScale.
  u doneAction: doneAction.
  u envelope: envelope.
  ^(u * mul)!
 gate: gate levelScale: levelScale levelBias: levelBias timeScale: timeScale doneAction: doneAction envelope: envelope mul: mul add: add
  |u|
  u := self new.
  u gate: gate.
  u levelScale: levelScale.
  u levelBias: levelBias.
  u timeScale: timeScale.
  u doneAction: doneAction.
  u envelope: envelope.
  ^(u * mul + add)!
!

UGen subclass: #ExpRand
 instanceVariableNames: 'lo hi'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

ExpRand comment: 'Exponential single random number generator.'!

!ExpRand methodsFor: 'accessing'!
 lo ^lo!
 lo: x lo := x!
 hi ^hi!
 hi: x hi := x!
!

!ExpRand methodsFor: 'initialization'!
 initialize
  ugenName := #ExpRand.
  paramNames := #(#lo #hi).
  mceNames := #().
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  lo := 0.01. hi := 1.0!
!

!ExpRand class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!ExpRand class methodsFor: 'instance creation'!
 lo: lo hi: hi
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^u!
 lo: lo hi: hi mul: mul
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul)!
 lo: lo hi: hi mul: mul add: add
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul + add)!
!

UGen subclass: #Formant
 instanceVariableNames: 'fundfreq formfreq bwfreq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Formant comment: 'Formant oscillator'!

!Formant methodsFor: 'accessing'!
 fundfreq ^fundfreq!
 fundfreq: x fundfreq := x!
 formfreq ^formfreq!
 formfreq: x formfreq := x!
 bwfreq ^bwfreq!
 bwfreq: x bwfreq := x!
!

!Formant methodsFor: 'initialization'!
 initialize
  ugenName := #Formant.
  paramNames := #(#fundfreq #formfreq #bwfreq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  fundfreq := 440.0. formfreq := 1760.0. bwfreq := 880.0!
!

!Formant class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Formant class methodsFor: 'instance creation'!
 fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq
  |u|
  u := self new.
  u fundfreq: fundfreq.
  u formfreq: formfreq.
  u bwfreq: bwfreq.
  ^u!
 fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul
  |u|
  u := self new.
  u fundfreq: fundfreq.
  u formfreq: formfreq.
  u bwfreq: bwfreq.
  ^(u * mul)!
 fundfreq: fundfreq formfreq: formfreq bwfreq: bwfreq mul: mul add: add
  |u|
  u := self new.
  u fundfreq: fundfreq.
  u formfreq: formfreq.
  u bwfreq: bwfreq.
  ^(u * mul + add)!
!

UGen subclass: #FSinOsc
 instanceVariableNames: 'freq iphase'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

FSinOsc comment: 'Fast sine oscillator.'!

!FSinOsc methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 iphase ^iphase!
 iphase: x iphase := x!
!

!FSinOsc methodsFor: 'initialization'!
 initialize
  ugenName := #FSinOsc.
  paramNames := #(#freq #iphase).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. iphase := 0.0!
!

!FSinOsc class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!FSinOsc class methodsFor: 'instance creation'!
 freq: freq iphase: iphase
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^u!
 freq: freq iphase: iphase mul: mul
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul)!
 freq: freq iphase: iphase mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul + add)!
!

UGen subclass: #FreeVerb
 instanceVariableNames: 'in mix room damp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

FreeVerb comment: 'A reverb'!

!FreeVerb methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 mix ^mix!
 mix: x mix := x!
 room ^room!
 room: x room := x!
 damp ^damp!
 damp: x damp := x!
!

!FreeVerb methodsFor: 'initialization'!
 initialize
  ugenName := #FreeVerb.
  paramNames := #(#in #mix #room #damp).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. mix := 0.33. room := 0.5. damp := 0.5!
!

!FreeVerb class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!FreeVerb class methodsFor: 'instance creation'!
 in: in mix: mix room: room damp: damp
  |u|
  u := self new.
  u in: in.
  u mix: mix.
  u room: room.
  u damp: damp.
  ^u!
 in: in mix: mix room: room damp: damp mul: mul
  |u|
  u := self new.
  u in: in.
  u mix: mix.
  u room: room.
  u damp: damp.
  ^(u * mul)!
 in: in mix: mix room: room damp: damp mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u mix: mix.
  u room: room.
  u damp: damp.
  ^(u * mul + add)!
!

UGen subclass: #GVerb
 instanceVariableNames: 'in roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

GVerb comment: 'A two-channel reverb'!

!GVerb methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 roomsize ^roomsize!
 roomsize: x roomsize := x!
 revtime ^revtime!
 revtime: x revtime := x!
 damping ^damping!
 damping: x damping := x!
 inputbw ^inputbw!
 inputbw: x inputbw := x!
 spread ^spread!
 spread: x spread := x!
 drylevel ^drylevel!
 drylevel: x drylevel := x!
 earlyreflevel ^earlyreflevel!
 earlyreflevel: x earlyreflevel := x!
 taillevel ^taillevel!
 taillevel: x taillevel := x!
 maxroomsize ^maxroomsize!
 maxroomsize: x maxroomsize := x!
!

!GVerb methodsFor: 'initialization'!
 initialize
  ugenName := #GVerb.
  paramNames := #(#in #roomsize #revtime #damping #inputbw #spread #drylevel #earlyreflevel #taillevel #maxroomsize).
  mceNames := #().
  ugenRate := #ar.
  numChan := 2.
  specialIndex := 0.
  in := 0.0. roomsize := 10.0. revtime := 3.0. damping := 0.5. inputbw := 0.5. spread := 15.0. drylevel := 1.0. earlyreflevel := 0.7. taillevel := 0.5. maxroomsize := 300.0!
!

!GVerb class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!GVerb class methodsFor: 'instance creation'!
 in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize
  |u|
  u := self new.
  u in: in.
  u roomsize: roomsize.
  u revtime: revtime.
  u damping: damping.
  u inputbw: inputbw.
  u spread: spread.
  u drylevel: drylevel.
  u earlyreflevel: earlyreflevel.
  u taillevel: taillevel.
  u maxroomsize: maxroomsize.
  ^u!
 in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul
  |u|
  u := self new.
  u in: in.
  u roomsize: roomsize.
  u revtime: revtime.
  u damping: damping.
  u inputbw: inputbw.
  u spread: spread.
  u drylevel: drylevel.
  u earlyreflevel: earlyreflevel.
  u taillevel: taillevel.
  u maxroomsize: maxroomsize.
  ^(u * mul)!
 in: in roomsize: roomsize revtime: revtime damping: damping inputbw: inputbw spread: spread drylevel: drylevel earlyreflevel: earlyreflevel taillevel: taillevel maxroomsize: maxroomsize mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u roomsize: roomsize.
  u revtime: revtime.
  u damping: damping.
  u inputbw: inputbw.
  u spread: spread.
  u drylevel: drylevel.
  u earlyreflevel: earlyreflevel.
  u taillevel: taillevel.
  u maxroomsize: maxroomsize.
  ^(u * mul + add)!
!

UGen subclass: #HPF
 instanceVariableNames: 'in freq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

HPF comment: '2nd order Butterworth highpass filter.'!

!HPF methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
!

!HPF methodsFor: 'initialization'!
 initialize
  ugenName := #HPF.
  paramNames := #(#in #freq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0!
!

!HPF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!HPF class methodsFor: 'instance creation'!
 in: in freq: freq
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^u!
 in: in freq: freq mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^(u * mul)!
 in: in freq: freq mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^(u * mul + add)!
!

UGen subclass: #HPZ1
 instanceVariableNames: 'in'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

HPZ1 comment: 'Two point difference filter'!

!HPZ1 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
!

!HPZ1 methodsFor: 'initialization'!
 initialize
  ugenName := #HPZ1.
  paramNames := #(#in).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0!
!

!HPZ1 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!HPZ1 class methodsFor: 'instance creation'!
 in: in
  |u|
  u := self new.
  u in: in.
  ^u!
 in: in mul: mul
  |u|
  u := self new.
  u in: in.
  ^(u * mul)!
 in: in mul: mul add: add
  |u|
  u := self new.
  u in: in.
  ^(u * mul + add)!
!

UGen subclass: #IRand
 instanceVariableNames: 'lo hi'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

IRand comment: 'Single integer random number generator.'!

!IRand methodsFor: 'accessing'!
 lo ^lo!
 lo: x lo := x!
 hi ^hi!
 hi: x hi := x!
!

!IRand methodsFor: 'initialization'!
 initialize
  ugenName := #IRand.
  paramNames := #(#lo #hi).
  mceNames := #().
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  lo := 0.0. hi := 127.0!
!

!IRand class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!IRand class methodsFor: 'instance creation'!
 lo: lo hi: hi
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^u!
 lo: lo hi: hi mul: mul
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul)!
 lo: lo hi: hi mul: mul add: add
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul + add)!
!

UGen subclass: #Impulse
 instanceVariableNames: 'freq phase'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Impulse comment: 'Impulse oscillator.'!

!Impulse methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 phase ^phase!
 phase: x phase := x!
!

!Impulse methodsFor: 'initialization'!
 initialize
  ugenName := #Impulse.
  paramNames := #(#freq #phase).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. phase := 0.0!
!

!Impulse class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Impulse class methodsFor: 'instance creation'!
 freq: freq phase: phase
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^u!
 freq: freq phase: phase mul: mul
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^(u * mul)!
 freq: freq phase: phase mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^(u * mul + add)!
!

UGen subclass: #In
 instanceVariableNames: 'bus'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

In comment: 'Read a signal from a bus.'!

!In methodsFor: 'accessing'!
 bus ^bus!
 bus: x bus := x!
!

!In methodsFor: 'initialization'!
 initialize
  ugenName := #In.
  paramNames := #(#bus).
  mceNames := #().
  ugenRate := #ar.
  numChan := nil.
  specialIndex := 0.
  bus := 0.0!
!

!In class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!In class methodsFor: 'instance creation'!
 numChan: numChan bus: bus
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^u!
 numChan: numChan bus: bus mul: mul
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^(u * mul)!
 numChan: numChan bus: bus mul: mul add: add
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^(u * mul + add)!
!

UGen subclass: #InFeedback
 instanceVariableNames: 'bus'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

InFeedback comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFeedback methodsFor: 'accessing'!
 bus ^bus!
 bus: x bus := x!
!

!InFeedback methodsFor: 'initialization'!
 initialize
  ugenName := #InFeedback.
  paramNames := #(#bus).
  mceNames := #().
  ugenRate := #ar.
  numChan := nil.
  specialIndex := 0.
  bus := 0.0!
!

!InFeedback class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!InFeedback class methodsFor: 'instance creation'!
 numChan: numChan bus: bus
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^u!
 numChan: numChan bus: bus mul: mul
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^(u * mul)!
 numChan: numChan bus: bus mul: mul add: add
  |u|
  u := self new.
  u numChan: numChan.
  u bus: bus.
  ^(u * mul + add)!
!

UGen subclass: #Klank
 instanceVariableNames: 'input freqscale freqoffset decayscale specificationsArrayRef'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Klank comment: 'Bank of resonators'!

!Klank methodsFor: 'accessing'!
 input ^input!
 input: x input := x!
 freqscale ^freqscale!
 freqscale: x freqscale := x!
 freqoffset ^freqoffset!
 freqoffset: x freqoffset := x!
 decayscale ^decayscale!
 decayscale: x decayscale := x!
 specificationsArrayRef ^specificationsArrayRef!
 specificationsArrayRef: x specificationsArrayRef := x!
!

!Klank methodsFor: 'initialization'!
 initialize
  ugenName := #Klank.
  paramNames := #(#input #freqscale #freqoffset #decayscale).
  mceNames := #(#specificationsArrayRef).
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  input := 0.0. freqscale := 1.0. freqoffset := 0.0. decayscale := 1.0!
!

!Klank class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Klank class methodsFor: 'instance creation'!
 input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
  |u|
  u := self new.
  u input: input.
  u freqscale: freqscale.
  u freqoffset: freqoffset.
  u decayscale: decayscale.
  u specificationsArrayRef: specificationsArrayRef.
  ^u!
 input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
  |u|
  u := self new.
  u input: input.
  u freqscale: freqscale.
  u freqoffset: freqoffset.
  u decayscale: decayscale.
  u specificationsArrayRef: specificationsArrayRef.
  ^(u * mul)!
 input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul add: add
  |u|
  u := self new.
  u input: input.
  u freqscale: freqscale.
  u freqoffset: freqoffset.
  u decayscale: decayscale.
  u specificationsArrayRef: specificationsArrayRef.
  ^(u * mul + add)!
!

UGen subclass: #LFNoise1
 instanceVariableNames: 'freq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LFNoise1 comment: 'Ramp noise'!

!LFNoise1 methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
!

!LFNoise1 methodsFor: 'initialization'!
 initialize
  ugenName := #LFNoise1.
  paramNames := #(#freq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 500.0!
!

!LFNoise1 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LFNoise1 class methodsFor: 'instance creation'!
 freq: freq
  |u|
  u := self new.
  u freq: freq.
  ^u!
 freq: freq mul: mul
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul)!
 freq: freq mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul + add)!
!

UGen subclass: #LFNoise2
 instanceVariableNames: 'freq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LFNoise2 comment: 'Quadratic noise.'!

!LFNoise2 methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
!

!LFNoise2 methodsFor: 'initialization'!
 initialize
  ugenName := #LFNoise2.
  paramNames := #(#freq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 500.0!
!

!LFNoise2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LFNoise2 class methodsFor: 'instance creation'!
 freq: freq
  |u|
  u := self new.
  u freq: freq.
  ^u!
 freq: freq mul: mul
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul)!
 freq: freq mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul + add)!
!

UGen subclass: #LFPulse
 instanceVariableNames: 'freq iphase width'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LFPulse comment: 'pulse oscillator'!

!LFPulse methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 iphase ^iphase!
 iphase: x iphase := x!
 width ^width!
 width: x width := x!
!

!LFPulse methodsFor: 'initialization'!
 initialize
  ugenName := #LFPulse.
  paramNames := #(#freq #iphase #width).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. iphase := 0.0. width := 0.5!
!

!LFPulse class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LFPulse class methodsFor: 'instance creation'!
 freq: freq iphase: iphase width: width
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^u!
 freq: freq iphase: iphase width: width mul: mul
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^(u * mul)!
 freq: freq iphase: iphase width: width mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^(u * mul + add)!
!

UGen subclass: #LFSaw
 instanceVariableNames: 'freq iphase'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LFSaw comment: 'Sawtooth oscillator'!

!LFSaw methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 iphase ^iphase!
 iphase: x iphase := x!
!

!LFSaw methodsFor: 'initialization'!
 initialize
  ugenName := #LFSaw.
  paramNames := #(#freq #iphase).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. iphase := 0.0!
!

!LFSaw class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LFSaw class methodsFor: 'instance creation'!
 freq: freq iphase: iphase
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^u!
 freq: freq iphase: iphase mul: mul
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul)!
 freq: freq iphase: iphase mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul + add)!
!

UGen subclass: #LFTri
 instanceVariableNames: 'freq iphase'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LFTri comment: 'Triangle oscillator'!

!LFTri methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 iphase ^iphase!
 iphase: x iphase := x!
!

!LFTri methodsFor: 'initialization'!
 initialize
  ugenName := #LFTri.
  paramNames := #(#freq #iphase).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. iphase := 0.0!
!

!LFTri class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LFTri class methodsFor: 'instance creation'!
 freq: freq iphase: iphase
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^u!
 freq: freq iphase: iphase mul: mul
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul)!
 freq: freq iphase: iphase mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  ^(u * mul + add)!
!

UGen subclass: #LPF
 instanceVariableNames: 'in freq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LPF comment: '2nd order Butterworth lowpass filter'!

!LPF methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
!

!LPF methodsFor: 'initialization'!
 initialize
  ugenName := #LPF.
  paramNames := #(#in #freq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0!
!

!LPF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LPF class methodsFor: 'instance creation'!
 in: in freq: freq
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^u!
 in: in freq: freq mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^(u * mul)!
 in: in freq: freq mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  ^(u * mul + add)!
!

UGen subclass: #Lag
 instanceVariableNames: 'in lagTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Lag comment: 'Exponential lag'!

!Lag methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 lagTime ^lagTime!
 lagTime: x lagTime := x!
!

!Lag methodsFor: 'initialization'!
 initialize
  ugenName := #Lag.
  paramNames := #(#in #lagTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. lagTime := 0.1!
!

!Lag class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Lag class methodsFor: 'instance creation'!
 in: in lagTime: lagTime
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^u!
 in: in lagTime: lagTime mul: mul
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul)!
 in: in lagTime: lagTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul + add)!
!

UGen subclass: #LagUD
 instanceVariableNames: 'in lagTimeU lagTimeD'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LagUD comment: 'Exponential lag'!

!LagUD methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 lagTimeU ^lagTimeU!
 lagTimeU: x lagTimeU := x!
 lagTimeD ^lagTimeD!
 lagTimeD: x lagTimeD := x!
!

!LagUD methodsFor: 'initialization'!
 initialize
  ugenName := #LagUD.
  paramNames := #(#in #lagTimeU #lagTimeD).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. lagTimeU := 0.1. lagTimeD := 0.1!
!

!LagUD class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LagUD class methodsFor: 'instance creation'!
 in: in lagTimeU: lagTimeU lagTimeD: lagTimeD
  |u|
  u := self new.
  u in: in.
  u lagTimeU: lagTimeU.
  u lagTimeD: lagTimeD.
  ^u!
 in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul
  |u|
  u := self new.
  u in: in.
  u lagTimeU: lagTimeU.
  u lagTimeD: lagTimeD.
  ^(u * mul)!
 in: in lagTimeU: lagTimeU lagTimeD: lagTimeD mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u lagTimeU: lagTimeU.
  u lagTimeD: lagTimeD.
  ^(u * mul + add)!
!

UGen subclass: #Lag2
 instanceVariableNames: 'in lagTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Lag2 comment: 'Exponential lag'!

!Lag2 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 lagTime ^lagTime!
 lagTime: x lagTime := x!
!

!Lag2 methodsFor: 'initialization'!
 initialize
  ugenName := #Lag2.
  paramNames := #(#in #lagTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. lagTime := 0.1!
!

!Lag2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Lag2 class methodsFor: 'instance creation'!
 in: in lagTime: lagTime
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^u!
 in: in lagTime: lagTime mul: mul
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul)!
 in: in lagTime: lagTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul + add)!
!

UGen subclass: #Lag3
 instanceVariableNames: 'in lagTime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Lag3 comment: 'Exponential lag'!

!Lag3 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 lagTime ^lagTime!
 lagTime: x lagTime := x!
!

!Lag3 methodsFor: 'initialization'!
 initialize
  ugenName := #Lag3.
  paramNames := #(#in #lagTime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. lagTime := 0.1!
!

!Lag3 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Lag3 class methodsFor: 'instance creation'!
 in: in lagTime: lagTime
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^u!
 in: in lagTime: lagTime mul: mul
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul)!
 in: in lagTime: lagTime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u lagTime: lagTime.
  ^(u * mul + add)!
!

UGen subclass: #Latch
 instanceVariableNames: 'in trig'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Latch comment: 'Sample and hold'!

!Latch methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 trig ^trig!
 trig: x trig := x!
!

!Latch methodsFor: 'initialization'!
 initialize
  ugenName := #Latch.
  paramNames := #(#in #trig).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. trig := 0.0!
!

!Latch class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Latch class methodsFor: 'instance creation'!
 in: in trig: trig
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^u!
 in: in trig: trig mul: mul
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^(u * mul)!
 in: in trig: trig mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^(u * mul + add)!
!

UGen subclass: #LeakDC
 instanceVariableNames: 'in coef'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LeakDC comment: 'Remove DC'!

!LeakDC methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 coef ^coef!
 coef: x coef := x!
!

!LeakDC methodsFor: 'initialization'!
 initialize
  ugenName := #LeakDC.
  paramNames := #(#in #coef).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. coef := 0.995!
!

!LeakDC class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LeakDC class methodsFor: 'instance creation'!
 in: in coef: coef
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^u!
 in: in coef: coef mul: mul
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^(u * mul)!
 in: in coef: coef mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^(u * mul + add)!
!

UGen subclass: #Line
 instanceVariableNames: 'start end dur doneAction'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Line comment: 'Line generator.'!

!Line methodsFor: 'accessing'!
 start ^start!
 start: x start := x!
 end ^end!
 end: x end := x!
 dur ^dur!
 dur: x dur := x!
 doneAction ^doneAction!
 doneAction: x doneAction := x!
!

!Line methodsFor: 'initialization'!
 initialize
  ugenName := #Line.
  paramNames := #(#start #end #dur #doneAction).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  start := 0.0. end := 1.0. dur := 1.0. doneAction := 0.0!
!

!Line class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Line class methodsFor: 'instance creation'!
 start: start end: end dur: dur doneAction: doneAction
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^u!
 start: start end: end dur: dur doneAction: doneAction mul: mul
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^(u * mul)!
 start: start end: end dur: dur doneAction: doneAction mul: mul add: add
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^(u * mul + add)!
!

UGen subclass: #LinExp
 instanceVariableNames: 'in srclo srchi dstlo dsthi'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LinExp comment: 'Map a linear range to an exponential range'!

!LinExp methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 srclo ^srclo!
 srclo: x srclo := x!
 srchi ^srchi!
 srchi: x srchi := x!
 dstlo ^dstlo!
 dstlo: x dstlo := x!
 dsthi ^dsthi!
 dsthi: x dsthi := x!
!

!LinExp methodsFor: 'initialization'!
 initialize
  ugenName := #LinExp.
  paramNames := #(#in #srclo #srchi #dstlo #dsthi).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. srclo := 0.0. srchi := 1.0. dstlo := 1.0. dsthi := 2.0!
!

!LinExp class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LinExp class methodsFor: 'instance creation'!
 in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
  |u|
  u := self new.
  u in: in.
  u srclo: srclo.
  u srchi: srchi.
  u dstlo: dstlo.
  u dsthi: dsthi.
  ^u!
 in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul
  |u|
  u := self new.
  u in: in.
  u srclo: srclo.
  u srchi: srchi.
  u dstlo: dstlo.
  u dsthi: dsthi.
  ^(u * mul)!
 in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u srclo: srclo.
  u srchi: srchi.
  u dstlo: dstlo.
  u dsthi: dsthi.
  ^(u * mul + add)!
!

UGen subclass: #LocalBuf
 instanceVariableNames: 'numChannels numFrames'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LocalBuf comment: 'Allocate a buffer local to the synth'!

!LocalBuf methodsFor: 'accessing'!
 numChannels ^numChannels!
 numChannels: x numChannels := x!
 numFrames ^numFrames!
 numFrames: x numFrames := x!
!

!LocalBuf methodsFor: 'initialization'!
 initialize
  ugenName := #LocalBuf.
  paramNames := #(#numChannels #numFrames).
  mceNames := #().
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  numChannels := 1.0. numFrames := 1.0!
!

!LocalBuf class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LocalBuf class methodsFor: 'instance creation'!
 numChannels: numChannels numFrames: numFrames
  |u|
  u := self new.
  u numChannels: numChannels.
  u numFrames: numFrames.
  ^u!
 numChannels: numChannels numFrames: numFrames mul: mul
  |u|
  u := self new.
  u numChannels: numChannels.
  u numFrames: numFrames.
  ^(u * mul)!
 numChannels: numChannels numFrames: numFrames mul: mul add: add
  |u|
  u := self new.
  u numChannels: numChannels.
  u numFrames: numFrames.
  ^(u * mul + add)!
!

UGen subclass: #LocalIn
 instanceVariableNames: 'default'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LocalIn comment: 'Define and read from buses local to a synth.'!

!LocalIn methodsFor: 'accessing'!
 default ^default!
 default: x default := x!
!

!LocalIn methodsFor: 'initialization'!
 initialize
  ugenName := #LocalIn.
  paramNames := #().
  mceNames := #(#default).
  ugenRate := #ar.
  numChan := nil.
  specialIndex := 0.
  !
!

!LocalIn class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LocalIn class methodsFor: 'instance creation'!
 numChan: numChan default: default
  |u|
  u := self new.
  u numChan: numChan.
  u default: default.
  ^u!
 numChan: numChan default: default mul: mul
  |u|
  u := self new.
  u numChan: numChan.
  u default: default.
  ^(u * mul)!
 numChan: numChan default: default mul: mul add: add
  |u|
  u := self new.
  u numChan: numChan.
  u default: default.
  ^(u * mul + add)!
!

UGen subclass: #LocalOut
 instanceVariableNames: 'channelsArray'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

LocalOut comment: 'Write to buses local to a synth.'!

!LocalOut methodsFor: 'accessing'!
 channelsArray ^channelsArray!
 channelsArray: x channelsArray := x!
!

!LocalOut methodsFor: 'initialization'!
 initialize
  ugenName := #LocalOut.
  paramNames := #().
  mceNames := #(#channelsArray).
  ugenRate := #ar.
  numChan := 0.
  specialIndex := 0.
  !
!

!LocalOut class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!LocalOut class methodsFor: 'instance creation'!
 channelsArray: channelsArray
  |u|
  u := self new.
  u channelsArray: channelsArray.
  ^u!
 channelsArray: channelsArray mul: mul
  |u|
  u := self new.
  u channelsArray: channelsArray.
  ^(u * mul)!
 channelsArray: channelsArray mul: mul add: add
  |u|
  u := self new.
  u channelsArray: channelsArray.
  ^(u * mul + add)!
!

UGen subclass: #ModDif
 instanceVariableNames: 'x y mod'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

ModDif comment: 'Minimum difference of two values in modulo arithmetics'!

!ModDif methodsFor: 'accessing'!
 x ^x!
 x: i x := i!
 y ^y!
 y: x y := x!
 mod ^mod!
 mod: x mod := x!
!

!ModDif methodsFor: 'initialization'!
 initialize
  ugenName := #ModDif.
  paramNames := #(#x #y #mod).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  x := 0.0. y := 0.0. mod := 1.0!
!

!ModDif class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!ModDif class methodsFor: 'instance creation'!
 x: x y: y mod: mod
  |u|
  u := self new.
  u x: x.
  u y: y.
  u mod: mod.
  ^u!
 x: x y: y mod: mod mul: mul
  |u|
  u := self new.
  u x: x.
  u y: y.
  u mod: mod.
  ^(u * mul)!
 x: x y: y mod: mod mul: mul add: add
  |u|
  u := self new.
  u x: x.
  u y: y.
  u mod: mod.
  ^(u * mul + add)!
!

UGen subclass: #MouseX
 instanceVariableNames: 'minval maxval warp lag'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

MouseX comment: 'Cursor tracking UGen.'!

!MouseX methodsFor: 'accessing'!
 minval ^minval!
 minval: x minval := x!
 maxval ^maxval!
 maxval: x maxval := x!
 warp ^warp!
 warp: x warp := x!
 lag ^lag!
 lag: x lag := x!
!

!MouseX methodsFor: 'initialization'!
 initialize
  ugenName := #MouseX.
  paramNames := #(#minval #maxval #warp #lag).
  mceNames := #().
  ugenRate := #kr.
  numChan := 1.
  specialIndex := 0.
  minval := 0.0. maxval := 1.0. warp := 0.0. lag := 0.2!
!

!MouseX class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!MouseX class methodsFor: 'instance creation'!
 minval: minval maxval: maxval warp: warp lag: lag
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^u!
 minval: minval maxval: maxval warp: warp lag: lag mul: mul
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^(u * mul)!
 minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^(u * mul + add)!
!

UGen subclass: #MouseY
 instanceVariableNames: 'minval maxval warp lag'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

MouseY comment: 'Cursor tracking UGen.'!

!MouseY methodsFor: 'accessing'!
 minval ^minval!
 minval: x minval := x!
 maxval ^maxval!
 maxval: x maxval := x!
 warp ^warp!
 warp: x warp := x!
 lag ^lag!
 lag: x lag := x!
!

!MouseY methodsFor: 'initialization'!
 initialize
  ugenName := #MouseY.
  paramNames := #(#minval #maxval #warp #lag).
  mceNames := #().
  ugenRate := #kr.
  numChan := 1.
  specialIndex := 0.
  minval := 0.0. maxval := 1.0. warp := 0.0. lag := 0.2!
!

!MouseY class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!MouseY class methodsFor: 'instance creation'!
 minval: minval maxval: maxval warp: warp lag: lag
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^u!
 minval: minval maxval: maxval warp: warp lag: lag mul: mul
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^(u * mul)!
 minval: minval maxval: maxval warp: warp lag: lag mul: mul add: add
  |u|
  u := self new.
  u minval: minval.
  u maxval: maxval.
  u warp: warp.
  u lag: lag.
  ^(u * mul + add)!
!

UGen subclass: #MulAdd
 instanceVariableNames: 'in mul add'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

MulAdd comment: 'Multiply add'!

!MulAdd methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 mul ^mul!
 mul: x mul := x!
 add ^add!
 add: x add := x!
!

!MulAdd methodsFor: 'initialization'!
 initialize
  ugenName := #MulAdd.
  paramNames := #(#in #mul #add).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. mul := 0.0. add := 0.0!
!

!MulAdd class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!MulAdd class methodsFor: 'instance creation'!
 in: in mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u mul: mul.
  u add: add.
  ^u!
!

UGen subclass: #OnePole
 instanceVariableNames: 'in coef'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

OnePole comment: 'One pole filter.'!

!OnePole methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 coef ^coef!
 coef: x coef := x!
!

!OnePole methodsFor: 'initialization'!
 initialize
  ugenName := #OnePole.
  paramNames := #(#in #coef).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. coef := 0.5!
!

!OnePole class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!OnePole class methodsFor: 'instance creation'!
 in: in coef: coef
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^u!
 in: in coef: coef mul: mul
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^(u * mul)!
 in: in coef: coef mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u coef: coef.
  ^(u * mul + add)!
!

UGen subclass: #Out
 instanceVariableNames: 'bus channelsArray'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Out comment: 'Write a signal to a bus.'!

!Out methodsFor: 'accessing'!
 bus ^bus!
 bus: x bus := x!
 channelsArray ^channelsArray!
 channelsArray: x channelsArray := x!
!

!Out methodsFor: 'initialization'!
 initialize
  ugenName := #Out.
  paramNames := #(#bus).
  mceNames := #(#channelsArray).
  ugenRate := #ar.
  numChan := 0.
  specialIndex := 0.
  bus := 0.0!
!

!Out class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Out class methodsFor: 'instance creation'!
 bus: bus channelsArray: channelsArray
  |u|
  u := self new.
  u bus: bus.
  u channelsArray: channelsArray.
  ^u!
 bus: bus channelsArray: channelsArray mul: mul
  |u|
  u := self new.
  u bus: bus.
  u channelsArray: channelsArray.
  ^(u * mul)!
 bus: bus channelsArray: channelsArray mul: mul add: add
  |u|
  u := self new.
  u bus: bus.
  u channelsArray: channelsArray.
  ^(u * mul + add)!
!

UGen subclass: #Pan2
 instanceVariableNames: 'in pos level'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Pan2 comment: 'Two channel equal power pan.'!

!Pan2 methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 pos ^pos!
 pos: x pos := x!
 level ^level!
 level: x level := x!
!

!Pan2 methodsFor: 'initialization'!
 initialize
  ugenName := #Pan2.
  paramNames := #(#in #pos #level).
  mceNames := #().
  ugenRate := #ar.
  numChan := 2.
  specialIndex := 0.
  in := 0.0. pos := 0.0. level := 1.0!
!

!Pan2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Pan2 class methodsFor: 'instance creation'!
 in: in pos: pos level: level
  |u|
  u := self new.
  u in: in.
  u pos: pos.
  u level: level.
  ^u!
 in: in pos: pos level: level mul: mul
  |u|
  u := self new.
  u in: in.
  u pos: pos.
  u level: level.
  ^(u * mul)!
 in: in pos: pos level: level mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u pos: pos.
  u level: level.
  ^(u * mul + add)!
!

UGen subclass: #Phasor
 instanceVariableNames: 'trig rate start end resetPos'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Phasor comment: 'A resettable linear ramp between two levels.'!

!Phasor methodsFor: 'accessing'!
 trig ^trig!
 trig: x trig := x!
 rate ^rate!
 rate: x rate := x!
 start ^start!
 start: x start := x!
 end ^end!
 end: x end := x!
 resetPos ^resetPos!
 resetPos: x resetPos := x!
!

!Phasor methodsFor: 'initialization'!
 initialize
  ugenName := #Phasor.
  paramNames := #(#trig #rate #start #end #resetPos).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  trig := 0.0. rate := 1.0. start := 0.0. end := 1.0. resetPos := 0.0!
!

!Phasor class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Phasor class methodsFor: 'instance creation'!
 trig: trig rate: rate start: start end: end resetPos: resetPos
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  u start: start.
  u end: end.
  u resetPos: resetPos.
  ^u!
 trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  u start: start.
  u end: end.
  u resetPos: resetPos.
  ^(u * mul)!
 trig: trig rate: rate start: start end: end resetPos: resetPos mul: mul add: add
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  u start: start.
  u end: end.
  u resetPos: resetPos.
  ^(u * mul + add)!
!

UGen subclass: #PinkNoise
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

PinkNoise comment: 'Pink Noise.'!


!PinkNoise methodsFor: 'initialization'!
 initialize
  ugenName := #PinkNoise.
  paramNames := #().
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  !
!

!PinkNoise class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!PinkNoise class methodsFor: 'instance creation'!
  mul: mul
  |u|
  u := self new.
  
  ^(u * mul)!
  mul: mul add: add
  |u|
  u := self new.
  
  ^(u * mul + add)!
!

UGen subclass: #Pluck
 instanceVariableNames: 'in trig maxdelaytime delaytime decaytime coef'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Pluck comment: 'A Karplus-Strong UGen'!

!Pluck methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 trig ^trig!
 trig: x trig := x!
 maxdelaytime ^maxdelaytime!
 maxdelaytime: x maxdelaytime := x!
 delaytime ^delaytime!
 delaytime: x delaytime := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
 coef ^coef!
 coef: x coef := x!
!

!Pluck methodsFor: 'initialization'!
 initialize
  ugenName := #Pluck.
  paramNames := #(#in #trig #maxdelaytime #delaytime #decaytime #coef).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. trig := 1.0. maxdelaytime := 0.2. delaytime := 0.2. decaytime := 1.0. coef := 0.5!
!

!Pluck class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Pluck class methodsFor: 'instance creation'!
 in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  u coef: coef.
  ^u!
 in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  u coef: coef.
  ^(u * mul)!
 in: in trig: trig maxdelaytime: maxdelaytime delaytime: delaytime decaytime: decaytime coef: coef mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  u maxdelaytime: maxdelaytime.
  u delaytime: delaytime.
  u decaytime: decaytime.
  u coef: coef.
  ^(u * mul + add)!
!

UGen subclass: #Pulse
 instanceVariableNames: 'freq width'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Pulse comment: 'Band limited pulse wave.'!

!Pulse methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 width ^width!
 width: x width := x!
!

!Pulse methodsFor: 'initialization'!
 initialize
  ugenName := #Pulse.
  paramNames := #(#freq #width).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. width := 0.5!
!

!Pulse class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Pulse class methodsFor: 'instance creation'!
 freq: freq width: width
  |u|
  u := self new.
  u freq: freq.
  u width: width.
  ^u!
 freq: freq width: width mul: mul
  |u|
  u := self new.
  u freq: freq.
  u width: width.
  ^(u * mul)!
 freq: freq width: width mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u width: width.
  ^(u * mul + add)!
!

UGen subclass: #PulseDivider
 instanceVariableNames: 'trig div start'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

PulseDivider comment: 'Pulse divider.'!

!PulseDivider methodsFor: 'accessing'!
 trig ^trig!
 trig: x trig := x!
 div ^div!
 div: x div := x!
 start ^start!
 start: x start := x!
!

!PulseDivider methodsFor: 'initialization'!
 initialize
  ugenName := #PulseDivider.
  paramNames := #(#trig #div #start).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  trig := 0.0. div := 2.0. start := 0.0!
!

!PulseDivider class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!PulseDivider class methodsFor: 'instance creation'!
 trig: trig div: div start: start
  |u|
  u := self new.
  u trig: trig.
  u div: div.
  u start: start.
  ^u!
 trig: trig div: div start: start mul: mul
  |u|
  u := self new.
  u trig: trig.
  u div: div.
  u start: start.
  ^(u * mul)!
 trig: trig div: div start: start mul: mul add: add
  |u|
  u := self new.
  u trig: trig.
  u div: div.
  u start: start.
  ^(u * mul + add)!
!

UGen subclass: #RHPF
 instanceVariableNames: 'in freq rq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

RHPF comment: 'A resonant high pass filter.'!

!RHPF methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
 rq ^rq!
 rq: x rq := x!
!

!RHPF methodsFor: 'initialization'!
 initialize
  ugenName := #RHPF.
  paramNames := #(#in #freq #rq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0. rq := 1.0!
!

!RHPF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!RHPF class methodsFor: 'instance creation'!
 in: in freq: freq rq: rq
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^u!
 in: in freq: freq rq: rq mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul)!
 in: in freq: freq rq: rq mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul + add)!
!

UGen subclass: #RLPF
 instanceVariableNames: 'in freq rq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

RLPF comment: 'A resonant low pass filter.'!

!RLPF methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
 rq ^rq!
 rq: x rq := x!
!

!RLPF methodsFor: 'initialization'!
 initialize
  ugenName := #RLPF.
  paramNames := #(#in #freq #rq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0. rq := 1.0!
!

!RLPF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!RLPF class methodsFor: 'instance creation'!
 in: in freq: freq rq: rq
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^u!
 in: in freq: freq rq: rq mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul)!
 in: in freq: freq rq: rq mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u rq: rq.
  ^(u * mul + add)!
!

UGen subclass: #Rand
 instanceVariableNames: 'lo hi'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Rand comment: 'Single random number generator.'!

!Rand methodsFor: 'accessing'!
 lo ^lo!
 lo: x lo := x!
 hi ^hi!
 hi: x hi := x!
!

!Rand methodsFor: 'initialization'!
 initialize
  ugenName := #Rand.
  paramNames := #(#lo #hi).
  mceNames := #().
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  lo := 0.0. hi := 1.0!
!

!Rand class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Rand class methodsFor: 'instance creation'!
 lo: lo hi: hi
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^u!
 lo: lo hi: hi mul: mul
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul)!
 lo: lo hi: hi mul: mul add: add
  |u|
  u := self new.
  u lo: lo.
  u hi: hi.
  ^(u * mul + add)!
!

UGen subclass: #Resonz
 instanceVariableNames: 'in freq bwr'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Resonz comment: 'Resonant filter.'!

!Resonz methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
 bwr ^bwr!
 bwr: x bwr := x!
!

!Resonz methodsFor: 'initialization'!
 initialize
  ugenName := #Resonz.
  paramNames := #(#in #freq #bwr).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0. bwr := 1.0!
!

!Resonz class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Resonz class methodsFor: 'instance creation'!
 in: in freq: freq bwr: bwr
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u bwr: bwr.
  ^u!
 in: in freq: freq bwr: bwr mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u bwr: bwr.
  ^(u * mul)!
 in: in freq: freq bwr: bwr mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u bwr: bwr.
  ^(u * mul + add)!
!

UGen subclass: #Ringz
 instanceVariableNames: 'in freq decaytime'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Ringz comment: 'Ringing filter.'!

!Ringz methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 freq ^freq!
 freq: x freq := x!
 decaytime ^decaytime!
 decaytime: x decaytime := x!
!

!Ringz methodsFor: 'initialization'!
 initialize
  ugenName := #Ringz.
  paramNames := #(#in #freq #decaytime).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. freq := 440.0. decaytime := 1.0!
!

!Ringz class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Ringz class methodsFor: 'instance creation'!
 in: in freq: freq decaytime: decaytime
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u decaytime: decaytime.
  ^u!
 in: in freq: freq decaytime: decaytime mul: mul
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u decaytime: decaytime.
  ^(u * mul)!
 in: in freq: freq decaytime: decaytime mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u freq: freq.
  u decaytime: decaytime.
  ^(u * mul + add)!
!

UGen subclass: #RunningMax
 instanceVariableNames: 'in trig'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

RunningMax comment: 'Track maximum level.'!

!RunningMax methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 trig ^trig!
 trig: x trig := x!
!

!RunningMax methodsFor: 'initialization'!
 initialize
  ugenName := #RunningMax.
  paramNames := #(#in #trig).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. trig := 0.0!
!

!RunningMax class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!RunningMax class methodsFor: 'instance creation'!
 in: in trig: trig
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^u!
 in: in trig: trig mul: mul
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^(u * mul)!
 in: in trig: trig mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u trig: trig.
  ^(u * mul + add)!
!

UGen subclass: #SampleDur
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

SampleDur comment: 'Duration of one sample.'!


!SampleDur methodsFor: 'initialization'!
 initialize
  ugenName := #SampleDur.
  paramNames := #().
  mceNames := #().
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  !
!

!SampleDur class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!SampleDur class methodsFor: 'instance creation'!
  mul: mul
  |u|
  u := self new.
  
  ^(u * mul)!
  mul: mul add: add
  |u|
  u := self new.
  
  ^(u * mul + add)!
!

UGen subclass: #Saw
 instanceVariableNames: 'freq'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Saw comment: 'Band limited sawtooth.'!

!Saw methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
!

!Saw methodsFor: 'initialization'!
 initialize
  ugenName := #Saw.
  paramNames := #(#freq).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0!
!

!Saw class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Saw class methodsFor: 'instance creation'!
 freq: freq
  |u|
  u := self new.
  u freq: freq.
  ^u!
 freq: freq mul: mul
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul)!
 freq: freq mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  ^(u * mul + add)!
!

UGen subclass: #Select
 instanceVariableNames: 'which array'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Select comment: 'Select output from an array of inputs.'!

!Select methodsFor: 'accessing'!
 which ^which!
 which: x which := x!
 array ^array!
 array: x array := x!
!

!Select methodsFor: 'initialization'!
 initialize
  ugenName := #Select.
  paramNames := #(#which).
  mceNames := #(#array).
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  which := 0.0!
!

!Select class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Select class methodsFor: 'instance creation'!
 which: which array: array
  |u|
  u := self new.
  u which: which.
  u array: array.
  ^u!
 which: which array: array mul: mul
  |u|
  u := self new.
  u which: which.
  u array: array.
  ^(u * mul)!
 which: which array: array mul: mul add: add
  |u|
  u := self new.
  u which: which.
  u array: array.
  ^(u * mul + add)!
!

UGen subclass: #SetBuf
 instanceVariableNames: 'buf offset length array'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

SetBuf comment: 'Set local buffer'!

!SetBuf methodsFor: 'accessing'!
 buf ^buf!
 buf: x buf := x!
 offset ^offset!
 offset: x offset := x!
 length ^length!
 length: x length := x!
 array ^array!
 array: x array := x!
!

!SetBuf methodsFor: 'initialization'!
 initialize
  ugenName := #SetBuf.
  paramNames := #(#buf #offset #length).
  mceNames := #(#array).
  ugenRate := #ir.
  numChan := 1.
  specialIndex := 0.
  buf := 0.0. offset := 0.0. length := 0.0!
!

!SetBuf class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!SetBuf class methodsFor: 'instance creation'!
 buf: buf offset: offset length: length array: array
  |u|
  u := self new.
  u buf: buf.
  u offset: offset.
  u length: length.
  u array: array.
  ^u!
 buf: buf offset: offset length: length array: array mul: mul
  |u|
  u := self new.
  u buf: buf.
  u offset: offset.
  u length: length.
  u array: array.
  ^(u * mul)!
 buf: buf offset: offset length: length array: array mul: mul add: add
  |u|
  u := self new.
  u buf: buf.
  u offset: offset.
  u length: length.
  u array: array.
  ^(u * mul + add)!
!

UGen subclass: #SetResetFF
 instanceVariableNames: 'trig reset'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

SetResetFF comment: 'Set-reset flip flop.'!

!SetResetFF methodsFor: 'accessing'!
 trig ^trig!
 trig: x trig := x!
 reset ^reset!
 reset: x reset := x!
!

!SetResetFF methodsFor: 'initialization'!
 initialize
  ugenName := #SetResetFF.
  paramNames := #(#trig #reset).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  trig := 0.0. reset := 0.0!
!

!SetResetFF class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!SetResetFF class methodsFor: 'instance creation'!
 trig: trig reset: reset
  |u|
  u := self new.
  u trig: trig.
  u reset: reset.
  ^u!
 trig: trig reset: reset mul: mul
  |u|
  u := self new.
  u trig: trig.
  u reset: reset.
  ^(u * mul)!
 trig: trig reset: reset mul: mul add: add
  |u|
  u := self new.
  u trig: trig.
  u reset: reset.
  ^(u * mul + add)!
!

UGen subclass: #SinOsc
 instanceVariableNames: 'freq phase'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

SinOsc comment: 'Interpolating sine wavetable oscillator.'!

!SinOsc methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 phase ^phase!
 phase: x phase := x!
!

!SinOsc methodsFor: 'initialization'!
 initialize
  ugenName := #SinOsc.
  paramNames := #(#freq #phase).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. phase := 0.0!
!

!SinOsc class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!SinOsc class methodsFor: 'instance creation'!
 freq: freq phase: phase
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^u!
 freq: freq phase: phase mul: mul
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^(u * mul)!
 freq: freq phase: phase mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u phase: phase.
  ^(u * mul + add)!
!

UGen subclass: #SinOscFB
 instanceVariableNames: 'freq feedback'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

SinOscFB comment: 'Feedback FM oscillator'!

!SinOscFB methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 feedback ^feedback!
 feedback: x feedback := x!
!

!SinOscFB methodsFor: 'initialization'!
 initialize
  ugenName := #SinOscFB.
  paramNames := #(#freq #feedback).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. feedback := 0.0!
!

!SinOscFB class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!SinOscFB class methodsFor: 'instance creation'!
 freq: freq feedback: feedback
  |u|
  u := self new.
  u freq: freq.
  u feedback: feedback.
  ^u!
 freq: freq feedback: feedback mul: mul
  |u|
  u := self new.
  u freq: freq.
  u feedback: feedback.
  ^(u * mul)!
 freq: freq feedback: feedback mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u feedback: feedback.
  ^(u * mul + add)!
!

UGen subclass: #Sweep
 instanceVariableNames: 'trig rate'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Sweep comment: 'Triggered linear ramp'!

!Sweep methodsFor: 'accessing'!
 trig ^trig!
 trig: x trig := x!
 rate ^rate!
 rate: x rate := x!
!

!Sweep methodsFor: 'initialization'!
 initialize
  ugenName := #Sweep.
  paramNames := #(#trig #rate).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  trig := 0.0. rate := 1.0!
!

!Sweep class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Sweep class methodsFor: 'instance creation'!
 trig: trig rate: rate
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  ^u!
 trig: trig rate: rate mul: mul
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  ^(u * mul)!
 trig: trig rate: rate mul: mul add: add
  |u|
  u := self new.
  u trig: trig.
  u rate: rate.
  ^(u * mul + add)!
!

UGen subclass: #Trig
 instanceVariableNames: 'in dur'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

Trig comment: 'Timed trigger.'!

!Trig methodsFor: 'accessing'!
 in ^in!
 in: x in := x!
 dur ^dur!
 dur: x dur := x!
!

!Trig methodsFor: 'initialization'!
 initialize
  ugenName := #Trig.
  paramNames := #(#in #dur).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  in := 0.0. dur := 0.1!
!

!Trig class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!Trig class methodsFor: 'instance creation'!
 in: in dur: dur
  |u|
  u := self new.
  u in: in.
  u dur: dur.
  ^u!
 in: in dur: dur mul: mul
  |u|
  u := self new.
  u in: in.
  u dur: dur.
  ^(u * mul)!
 in: in dur: dur mul: mul add: add
  |u|
  u := self new.
  u in: in.
  u dur: dur.
  ^(u * mul + add)!
!

UGen subclass: #VarSaw
 instanceVariableNames: 'freq iphase width'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

VarSaw comment: 'Variable duty saw'!

!VarSaw methodsFor: 'accessing'!
 freq ^freq!
 freq: x freq := x!
 iphase ^iphase!
 iphase: x iphase := x!
 width ^width!
 width: x width := x!
!

!VarSaw methodsFor: 'initialization'!
 initialize
  ugenName := #VarSaw.
  paramNames := #(#freq #iphase #width).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  freq := 440.0. iphase := 0.0. width := 0.5!
!

!VarSaw class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!VarSaw class methodsFor: 'instance creation'!
 freq: freq iphase: iphase width: width
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^u!
 freq: freq iphase: iphase width: width mul: mul
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^(u * mul)!
 freq: freq iphase: iphase width: width mul: mul add: add
  |u|
  u := self new.
  u freq: freq.
  u iphase: iphase.
  u width: width.
  ^(u * mul + add)!
!

UGen subclass: #WhiteNoise
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

WhiteNoise comment: 'White noise.'!


!WhiteNoise methodsFor: 'initialization'!
 initialize
  ugenName := #WhiteNoise.
  paramNames := #().
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  !
!

!WhiteNoise class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!WhiteNoise class methodsFor: 'instance creation'!
  mul: mul
  |u|
  u := self new.
  
  ^(u * mul)!
  mul: mul add: add
  |u|
  u := self new.
  
  ^(u * mul + add)!
!

UGen subclass: #XFade2
 instanceVariableNames: 'inA inB pan level'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

XFade2 comment: 'Equal power two channel cross fade.'!

!XFade2 methodsFor: 'accessing'!
 inA ^inA!
 inA: x inA := x!
 inB ^inB!
 inB: x inB := x!
 pan ^pan!
 pan: x pan := x!
 level ^level!
 level: x level := x!
!

!XFade2 methodsFor: 'initialization'!
 initialize
  ugenName := #XFade2.
  paramNames := #(#inA #inB #pan #level).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  inA := 0.0. inB := 0.0. pan := 0.0. level := 1.0!
!

!XFade2 class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!XFade2 class methodsFor: 'instance creation'!
 inA: inA inB: inB pan: pan level: level
  |u|
  u := self new.
  u inA: inA.
  u inB: inB.
  u pan: pan.
  u level: level.
  ^u!
 inA: inA inB: inB pan: pan level: level mul: mul
  |u|
  u := self new.
  u inA: inA.
  u inB: inB.
  u pan: pan.
  u level: level.
  ^(u * mul)!
 inA: inA inB: inB pan: pan level: level mul: mul add: add
  |u|
  u := self new.
  u inA: inA.
  u inB: inB.
  u pan: pan.
  u level: level.
  ^(u * mul + add)!
!

UGen subclass: #XLine
 instanceVariableNames: 'start end dur doneAction'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!

XLine comment: 'Exponential line generator.'!

!XLine methodsFor: 'accessing'!
 start ^start!
 start: x start := x!
 end ^end!
 end: x end := x!
 dur ^dur!
 dur: x dur := x!
 doneAction ^doneAction!
 doneAction: x doneAction := x!
!

!XLine methodsFor: 'initialization'!
 initialize
  ugenName := #XLine.
  paramNames := #(#start #end #dur #doneAction).
  mceNames := #().
  ugenRate := #ar.
  numChan := 1.
  specialIndex := 0.
  start := 1.0. end := 2.0. dur := 1.0. doneAction := 0.0!
!

!XLine class methodsFor: 'instance creation'!
 new ^ self basicNew initialize!
!

!XLine class methodsFor: 'instance creation'!
 start: start end: end dur: dur doneAction: doneAction
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^u!
 start: start end: end dur: dur doneAction: doneAction mul: mul
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^(u * mul)!
 start: start end: end dur: dur doneAction: doneAction mul: mul add: add
  |u|
  u := self new.
  u start: start.
  u end: end.
  u dur: dur.
  u doneAction: doneAction.
  ^(u * mul + add)!
!


!UGen methodsFor: 'arithmetic'!
+ b ^ BinaryOpUGen specialIndex: 0 a: self b: b!
- b ^ BinaryOpUGen specialIndex: 1 a: self b: b!
* b ^ BinaryOpUGen specialIndex: 2 a: self b: b!
/ b ^ BinaryOpUGen specialIndex: 4 a: self b: b!
% b ^ BinaryOpUGen specialIndex: 5 a: self b: b!
/= b ^ BinaryOpUGen specialIndex: 7 a: self b: b!
< b ^ BinaryOpUGen specialIndex: 8 a: self b: b!
> b ^ BinaryOpUGen specialIndex: 9 a: self b: b!
<= b ^ BinaryOpUGen specialIndex: 10 a: self b: b!
>= b ^ BinaryOpUGen specialIndex: 11 a: self b: b!
min: b ^ BinaryOpUGen specialIndex: 12 a: self b: b!
max: b ^ BinaryOpUGen specialIndex: 13 a: self b: b!
lcm: b ^ BinaryOpUGen specialIndex: 17 a: self b: b!
gcd: b ^ BinaryOpUGen specialIndex: 18 a: self b: b!
round: b ^ BinaryOpUGen specialIndex: 19 a: self b: b!
** b ^ BinaryOpUGen specialIndex: 25 a: self b: b!
amclip: b ^ BinaryOpUGen specialIndex: 40 a: self b: b!
clip2: b ^ BinaryOpUGen specialIndex: 42 a: self b: b!
!

!UGen methodsFor: 'arithmetic'!
negated ^ UnaryOpUGen specialIndex: 0 a: self!
abs ^ UnaryOpUGen specialIndex: 5 a: self!
frac ^ UnaryOpUGen specialIndex: 10 a: self!
cubed ^ UnaryOpUGen specialIndex: 13 a: self!
reciprocal ^ UnaryOpUGen specialIndex: 16 a: self!
midicps ^ UnaryOpUGen specialIndex: 17 a: self!
log ^ UnaryOpUGen specialIndex: 25 a: self!
sin ^ UnaryOpUGen specialIndex: 28 a: self!
tanh ^ UnaryOpUGen specialIndex: 36 a: self!
distort ^ UnaryOpUGen specialIndex: 42 a: self!
!

