!LocalBuf class methodsFor: 'instance creation'!
  newFrom: x
    | p q |
    p := self numChannels: 1 numFrames: x size.
    q := SetBuf buf: p offset: 0 length: x size array: x.
    ^Mrg lhs: p rhs: q
  !
!

AbstractUGen subclass: #Splay
  instanceVariableNames: 'input spread level center levelComp'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Splay comment: 'Pan a set of channels across the stereo field.'!

!Splay methodsFor: 'collecting'!
  addToUGenSet: c
    input addToUGenSet: c.
    spread addToUGenSet: c.
    level addToUGenSet: c.
    center addToUGenSet: c
  !
!

!Splay methodsFor: 'accessing'!
  center ^center!
  center: x center := x!
  input ^input!
  input: x input := x!
  level ^level!
  level: x level := x!
  levelComp ^levelComp!
  levelComp: x levelComp := x!
  spread ^spread!
  spread: x spread := x!
!

!Splay class methodsFor: 'instance creation'!
  input: input spread: spread level: level center: center levelComp: levelComp
    | u |
    u := self new.
    u input: input.
    u spread: spread.
    u level: level.
    u center: center.
    u levelComp: levelComp.
    ^u
  !
  primaryFactoryMethod
    ^#input:spread:level:center:levelComp:
  !
  input: input spread: spread level: level center: center
    ^Splay input: input spread: spread level: level center: center levelComp: true
  !
  input: input level: level
    ^Splay input: input spread: 1 level: level center: 0 levelComp: true
  !
  input: input
    ^Splay input: input spread: 1 level: 1 center: 0 levelComp: true
  !
!

!AbstractUGen methodsFor: 'panning'!
  splay
    ^Splay input: self
  !
!

!Array methodsFor: 'panning'!
  splay
    ^Splay input: self
  !
!

!AbstractUGen methodsFor: 'arithmetic'!
  inExpRangeFrom: x to: y
    "LinExp with input range of (-1,1)"
    ^LinExp in: self srclo: (-1) srchi: 1 dstlo: x dsthi: y
  !
  inRangeFrom: x to: y
    "LinLin with input range of (-1,1)"
    | m a |
    m := (y - x) / 2.
    a := x + m.
    ^self * m + a.
  !
  exprange: anAssoc
    "inExpRangeFrom:to: from Association"
    ^self inExpRangeFrom: anAssoc key to: anAssoc value
  !
  range: anAssoc
    "inRangeFrom:to: from Association"
    ^self inRangeFrom: anAssoc key to: anAssoc value
  !
  exprange: lo value: hi
    ".stc exprange"
    ^self inExpRangeFrom: lo to: hi
  !
  range: lo value: hi
    ".stc range"
    ^self inRangeFrom: lo to: hi
  !
!

!SequenceableCollection methodsFor: 'UGen creation'!
  asLocalBuf
    ^LocalBuf newFrom: self
  !
!

AbstractUGen subclass: #TLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

TLine comment: 'Triggered Line.'!

!TLine class methodsFor: 'instance creation'!
  start: start end: end dur: dur trig: trig
    | env |
    env := EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: (Array with: #lin with: #lin)
               releaseNode: -1 loopNode: -1 offset: 0.
    ^EnvGen gate: trig levelScale: 1 levelBias: 0 timeScale: 1 doneAction: 0 envelope: env coord
  !
  primaryFactoryMethod ^#start:end:dur:trig:!
!

AbstractUGen subclass: #TXLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

TXLine comment: 'Triggered XLine.'!

!TXLine class methodsFor: 'instance creation'!
  start: start end: end dur: dur trig: trig
    | env |
    env := EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: #(#exp #exp)
               releaseNode: -1 loopNode: -1 offset: 0.
    ^EnvGen gate: trig levelScale: 1 levelBias: 0 timeScale: 1 doneAction: 0 envelope: env coord
  !
  primaryFactoryMethod ^#start:end:dur:trig:!
!

UGen subclass: #TChoose
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

TChoose comment: 'Randomly select one of several inputs on trigger.'!

!TChoose class methodsFor: 'instance creation'!
  trig: trig array: array
    ^Select which: (TIRand lo: 0 hi: (array size - 1) trig: trig) array: array
  !
  primaryFactoryMethod ^#trig:array:!
!

UGen subclass: #PMOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

PMOsc comment: 'Phase modulation sine oscillator pair.'!

!PMOsc class methodsFor: 'instance creation'!
  carfreq: cf modfreq: mf pmindex: pm modphase: mp
    ^SinOsc freq: cf phase: (SinOsc freq: mf phase: mp) * pm
  !
  carfreq: cf modfreq: mf pmindex: pm modphase: mp mul: mul
    ^(self carfreq: cf modfreq: mf pmindex: pm modphase: mp) * mul
  !
  primaryFactoryMethod ^#carfreq:modfreq:pmindex:modphase:!
!

!AbstractUGen methodsFor: 'smoothing'!
  lag: lagTime
    ^Lag in: self lagTime: lagTime
  !
!

AbstractUGen subclass: #LinLin
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

LinLin comment: 'Map a linear range to a linear range'!

!LinLin class methodsFor: 'instance creation'!
  in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    ^(in - srclo) / (srchi - srclo) * (dsthi - dstlo) + dstlo
  !
  primaryFactoryMethod ^#in:srclo:srchi:dstlo:dsthi:!
!

!AbstractUGen methodsFor: 'buffer management'!
  clearBuf
    "ClearBuf does not copy the buffer number through so this is an Mrg node."
    ^Mrg lhs: self rhs: (ClearBuf buf: self)
  !
!

AbstractUGen subclass: #Select2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

Select2 comment: 'Select one of two branches based on predicate signal'!

!Select2 class methodsFor: 'instance creation'!
  predicate: predicate ifTrue: trueUGen ifFalse: falseUGen
    ^(predicate * (trueUGen - falseUGen)) + falseUGen
  !
  primaryFactoryMethod ^#predicate:ifTrue:ifFalse:!
!

!AbstractUGen methodsFor: 'selecting'!
  select2: trueUGen ifFalse: falseUGen ^Select2 predicate: self ifTrue: trueUGen ifFalse: falseUGen!
  select2: trueUGen value: falseUGen "For .stc" ^Select2 predicate: self ifTrue: trueUGen ifFalse: falseUGen!
!

AbstractUGen subclass: #KlankSpec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

KlankSpec comment: 'Format data for Klank UGen'!

!KlankSpec class methodsFor: 'instance creation'!
  freq: freq amp: amp time: time
    "Reshape input arrays, and allow amp and time to be nil (defaulting to 1)"
    | n |
    n := freq size.
    ^{freq. amp ifNil: [n replicate: 1]. time ifNil: [n replicate: 1]} extendToBeOfEqualSize transpose concatenation
  !
  primaryFactoryMethod ^#freq:amp:time:!
!

Array methodsFor: 'Klang data'!
  asKlangSpec ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
  asKlankSpec ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
!

AbstractUGen subclass: #DynKlank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

DynKlank comment: 'Dynamic klank, set of non-fixed resonating filters'!

!DynKlank class methodsFor: 'instance creation'!
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef
    | gen |
    gen := [ :ix |
        | f a d |
        f := specificationsArrayRef at: ix.
        a := specificationsArrayRef at: (ix + 1).
        d := specificationsArrayRef at: (ix + 2).
        Ringz in: input freq: f * freqscale + freqoffset decaytime: d * decayscale mul: a ].
    ^((1 to: specificationsArrayRef size by: 3) collect: gen) sum
  !
  input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    ^(self input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul
  !
  primaryFactoryMethod ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
!

AbstractUGen subclass: #SelectX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'!

SelectX comment: 'Mix one output from many sources'!

!SelectX class methodsFor: 'instance creation'!
  which: which array: array
      ^XFade2 inA: (Select which: (which roundTo: 2) array: array)
	      inB: (Select which: (which truncateTo: 2) + 1 array: array)
	      pan: ((which * 2 - 1) fold2: 1)
              level: 1
  !
  primaryFactoryMethod ^#which:array:!
!

AbstractUGen subclass: #SinOscBank instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

SinOscBank comment: 'Parallel bank of SinOsc'!

!SinOscBank class methodsFor: 'instance creation'!
  freq: freq amp: amp phase: phase
      ^Klang freqscale: 1 freqoffset: 0 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: phase)
  !
  primaryFactoryMethod ^#freq:amp:phase:!
!

AbstractUGen subclass: #RingzBank instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

RingzBank comment: 'Parallel bank of Ringz'!

!RingzBank class methodsFor: 'instance creation'!
  input: input freq: freq amp: amp time: time
      ^Klank input: input freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: time)
  !
  primaryFactoryMethod ^#input:freq:amp:time:!
!

AbstractUGen subclass: #Changed instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

Changed comment: 'Triggers when a value changes'!

!Changed class methodsFor: 'instance creation'!
  input: input threshold: threshold
      ^(HPZ1 in: input) abs > threshold
  !
  primaryFactoryMethod ^#input:threshold:!
!

AbstractUGen subclass: #BufRec instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

BufRec comment: 'Record signal into a Buffer'!

!BufRec class methodsFor: 'instance creation'!
  bufnum: bufnum reset: reset inputArray: inputArray
      ^RecordBuf bufnum: bufnum offset: 0 recLevel: 1 preLevel: 0 run: 1 loop: 1 trigger: reset doneAction: 0 inputArray: inputArray
  !
  primaryFactoryMethod ^#bufnum:reset:inputArray:!
!

AbstractUGen subclass: #Ln instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

Ln comment: 'Line generator'!

!Ln class methodsFor: 'instance creation'!
  start: start end: end dur: dur
      ^Line start: start end: end dur: dur doneAction: 0
  !
  primaryFactoryMethod ^#start:end:dur:!
!

AbstractUGen subclass: #XLn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

XLn comment: 'Line generator'!

!XLn class methodsFor: 'instance creation'!
  start: start end: end dur: dur
      ^Line start: start end: end dur: dur doneAction: 0
  !
  primaryFactoryMethod ^#start:end:dur:!
!

AbstractUGen subclass: #AudioIn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

AudioIn comment: 'Audio input'!

!AudioIn class methodsFor: 'instance creation'!
  channels: channels
      ^In numChan: 1 bus: NumOutputBuses new + channels - 1
  !
  primaryFactoryMethod ^#channels:!
!

AbstractUGen subclass: #Cc instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

Cc comment: 'Continuous controller input'!

!Cc class methodsFor: 'instance creation'!
  index: index
      ^(In numChan: 1 bus: 11000 + index) kr
  !
  primaryFactoryMethod ^#index:!
!

AbstractUGen subclass: #Sw instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

Sw comment: 'Switch (maintained or momentary) controller input'!

!Sw class methodsFor: 'instance creation'!
  index: index
      ^(In numChan: 1 bus: 12000 + index) kr
  !
  primaryFactoryMethod ^#index:!
!
