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
  input: input spread: spread level: level center: center
    ^Splay input: input spread: 1 level: 1 center: 0 levelComp: true
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
    env := Env levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: #(#lin #lin)
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
    env := Env levels: (Array with: start with: start with: end)
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
