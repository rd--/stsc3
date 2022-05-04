!LocalBuf class methodsFor: 'instance creation'!
newFrom: anArray
    | lhs rhs |
    lhs := self numChannels: 1 numFrames: anArray size.
    rhs := SetBuf buf: lhs offset: 0 length: anArray size array: anArray.
    ^Mrg lhs: lhs rhs: rhs! !

AbstractUgen subclass: #Splay instanceVariableNames: 'paramValues' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Splay comment: 'Pan a set of channels across the stereo field.'!

!Splay methodsFor: 'accessing'!
paramValues
    ^paramValues!
paramValues: anArray
    ^paramValues := anArray! !

!Splay methodsFor: 'collecting'!
addToUgenSet: aSet
    paramValues do: [ :item | item addToUgenSet: aSet ]! !

!Splay class methodsFor: 'instance creation'!
input: input spread: spread level: level center: center levelComp: levelComp
    | u |
    u := self new.
    u paramValues: {input. spread. level. center. levelComp}.
    ^u!
primaryFactoryMethod
    ^#input:spread:level:center:levelComp:!
input: input spread: spread level: level center: center
    ^Splay input: input spread: spread level: level center: center levelComp: true!
input: input level: level
    ^Splay input: input spread: 1 level: level center: 0 levelComp: true!
input: input
    ^Splay input: input spread: 1 level: 1 center: 0 levelComp: true! !

Ugen subclass: #Splay2 instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Splay2 comment: 'Pan an array of channels across the stereo field.'!

!Splay2 class methodsFor: 'instance creation'!
inArray: inArray
    ^Splay input: inArray spread: 1 level: 1 center: 0 levelComp: true!
primaryFactoryMethod
    ^#inArray:! !

!AbstractUgen methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self! !

!Array methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self! !

!AbstractUgen methodsFor: 'arithmetic'!
inExpRangeFrom: x to: y
    "LinExp with input range of (-1,1)"
    ^LinExp in: self srclo: (-1) srchi: 1 dstlo: x dsthi: y!
inRangeFrom: x to: y
    "LinLin with input range of (-1,1)"
    | m a |
    m := (y - x) / 2.
    a := x + m.
    ^self * m + a!
exprange: anAssoc
    "inExpRangeFrom:to: from Association"
    ^self inExpRangeFrom: anAssoc key to: anAssoc value!
range: anAssoc
    "inRangeFrom:to: from Association"
    ^self inRangeFrom: anAssoc key to: anAssoc value!
exprange: lo value: hi
    ".stc exprange"
    ^self inExpRangeFrom: lo to: hi!
range: lo value: hi
    ".stc range"
    ^self inRangeFrom: lo to: hi! !

!SequenceableCollection methodsFor: 'Ugen creation'!
asLocalBuf
    ^LocalBuf newFrom: self! !

AbstractUgen subclass: #TLine instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

TLine comment: 'Triggered Line.'!

!TLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: (Array with: #lin with: #lin)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:! !

AbstractUgen subclass: #TXLine instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

TXLine comment: 'Triggered XLine.'!

!TXLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: #(#exp #exp)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:! !

Ugen subclass: #TChoose instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

TChoose comment: 'Randomly select one of several inputs on trigger.'!

!TChoose class methodsFor: 'instance creation'!
trig: trig array: array
    ^Select which: (TIRand lo: 0 hi: (array size - 1) trig: trig) array: array!
primaryFactoryMethod
    ^#trig:array:! !

Ugen subclass: #PMOsc instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

PMOsc comment: 'Phase modulation sine oscillator pair.'!

!PMOsc class methodsFor: 'instance creation'!
carfreq: cf modfreq: mf pmindex: pm modphase: mp
    ^SinOsc freq: cf phase: (SinOsc freq: mf phase: mp) * pm!
carfreq: cf modfreq: mf pmindex: pm modphase: mp mul: mul
    ^(self carfreq: cf modfreq: mf pmindex: pm modphase: mp) * mul!
primaryFactoryMethod
    ^#carfreq:modfreq:pmindex:modphase:! !

!AbstractUgen methodsFor: 'smoothing'!
lag: lagTime
    ^Lag in: self lagTime: lagTime! !

AbstractUgen subclass: #LinLin instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

LinLin comment: 'Map a linear range to a linear range'!

!LinLin class methodsFor: 'instance creation'!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    ^(in - srclo) / (srchi - srclo) * (dsthi - dstlo) + dstlo!
primaryFactoryMethod
    ^#in:srclo:srchi:dstlo:dsthi:! !

!AbstractUgen methodsFor: 'buffer management'!
clearBuf
    "ClearBuf does not copy the buffer number through so this is an Mrg node."
    ^Mrg lhs: self rhs: (ClearBuf buf: self)! !

AbstractUgen subclass: #Select2 instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Select2 comment: 'Select one of two branches based on predicate signal'!

!Select2 class methodsFor: 'instance creation'!
predicate: predicate ifTrue: trueUgen ifFalse: falseUgen
    ^(predicate * (trueUgen - falseUgen)) + falseUgen!
primaryFactoryMethod
    ^#predicate:ifTrue:ifFalse:! !

!AbstractUgen methodsFor: 'selecting'!
select2: trueUgen ifFalse: falseUgen
    ^Select2 predicate: self ifTrue: trueUgen ifFalse: falseUgen!
select2: trueUgen value: falseUgen
    "For .stc notation"
    ^Select2 predicate: self ifTrue: trueUgen ifFalse: falseUgen! !

AbstractUgen subclass: #KlankSpec instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

KlankSpec comment: 'Format data for Klank Ugen'!

!KlankSpec class methodsFor: 'instance creation'!
freq: freq amp: amp time: time
    "Reshape input arrays, and allow amp and time to be nil (defaulting to 1)"
    | n |
    n := freq size.
    ^{freq. amp ifNil: [n replicate: 1]. time ifNil: [n replicate: 1]} extendToBeOfEqualSize transpose concatenation!
primaryFactoryMethod
    ^#freq:amp:time:! !

!Array methodsFor: 'Klang data'!
asKlangSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
asKlankSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)! !

AbstractUgen subclass: #DynKlank instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

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
    ^((1 to: specificationsArrayRef size by: 3) collect: gen) sum!
input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef mul: mul
    ^(self input: input freqscale: freqscale freqoffset: freqoffset decayscale: decayscale specificationsArrayRef: specificationsArrayRef) * mul!
primaryFactoryMethod
    ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:! !

AbstractUgen subclass: #SelectX instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

SelectX comment: 'Mix one output from many sources'!

!SelectX class methodsFor: 'instance creation'!
which: which array: array
    ^XFade2 inA: (Select which: (which roundTo: 2) array: array)
              inB: (Select which: (which truncateTo: 2) + 1 array: array)
              pan: ((which * 2 - 1) fold2: 1)
              level: 1!
primaryFactoryMethod
    ^#which:array:! !

AbstractUgen subclass: #SinOscBank instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

SinOscBank comment: 'Parallel bank of SinOsc'!

!SinOscBank class methodsFor: 'instance creation'!
freq: freq amp: amp phase: phase
    ^Klang freqscale: 1 freqoffset: 0 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: phase)!
primaryFactoryMethod
    ^#freq:amp:phase:! !

AbstractUgen subclass: #RingzBank instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

RingzBank comment: 'Parallel bank of Ringz'!

!RingzBank class methodsFor: 'instance creation'!
input: input freq: freq amp: amp time: time
    ^Klank input: input freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: time)!
primaryFactoryMethod
    ^#input:freq:amp:time:! !

AbstractUgen subclass: #Changed instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Changed comment: 'Triggers when a value changes'!

!Changed class methodsFor: 'instance creation'!
input: input threshold: threshold
    ^(HPZ1 in: input) abs > threshold!
primaryFactoryMethod
    ^#input:threshold:! !

AbstractUgen subclass: #BufRec instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

BufRec comment: 'Record signal into a Buffer'!

!BufRec class methodsFor: 'instance creation'!
bufnum: bufnum reset: reset inputArray: inputArray
    ^RecordBuf bufnum: bufnum offset: 0 recLevel: 1 preLevel: 0 run: 1 loop: 1 trigger: reset doneAction: 0 inputArray: inputArray!
primaryFactoryMethod
    ^#bufnum:reset:inputArray:! !

AbstractUgen subclass: #Ln instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Ln comment: 'Line generator'!

!Ln class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^Line start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:! !

AbstractUgen subclass: #XLn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

XLn comment: 'Line generator'!

!XLn class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^XLine start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:! !

AbstractUgen subclass: #AudioIn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

AudioIn comment: 'Audio input'!

!AudioIn class methodsFor: 'instance creation'!
channels: channels
    ^In numChan: 1 bus: NumOutputBuses new + channels - 1!
primaryFactoryMethod
    ^#channels:! !

AbstractUgen subclass: #ControlIn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

ControlIn comment: 'Control input'!

!ControlIn class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^(In numChan: numChan bus: bus) kr!
channels: channels
    ^In numChan: 1 bus: channels - 1!
primaryFactoryMethod
    ^#channels:! !

AbstractUgen subclass: #Cc instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Cc comment: 'Continuous controller input'!

!Cc class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 11000 + index) kr!
primaryFactoryMethod
    ^#index:! !

AbstractUgen subclass: #Sw instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Sw comment: 'Switch (maintained or momentary) controller input'!

!Sw class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 12000 + index) kr!
primaryFactoryMethod
    ^#index:! !

Ugen subclass: #InFb instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

InFb comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFb class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^InFeedback numChan: numChan bus: bus!
primaryFactoryMethod
    ^#numChan:bus:! !

Ugen subclass: #BufAlloc instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

BufAlloc comment: 'Allocate a buffer local to the synth.'!

!BufAlloc class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames
    ^LocalBuf numChannels: numChannels numFrames: numFrames!
primaryFactoryMethod
    ^#numChannels:numFrames:! !

Ugen subclass: #Tr instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Tr comment: 'Trig with duration of one sample.'!

!Tr class methodsFor: 'instance creation'!
in: in
    ^Trig in: in dur: SampleDur new "Should be SampleDur if audio rate and ControlDur if control rate, requires QuantaDur or some such."!
primaryFactoryMethod
    ^#in:! !

Ugen subclass: #Tr1 instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Tr1 comment: 'Trig1 with duration of one sample.'!

!Tr1 class methodsFor: 'instance creation'!
in: in
    ^Trig1 in: in dur: SampleDur new!
primaryFactoryMethod
    ^#in:! !

Ugen subclass: #Osc1 instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Osc1 comment: 'An oscillator that reads through a table once.'!

!Osc1 class methodsFor: 'instance creation'!
bufnum: bufnum dur: dur
    | phase |
    phase := Ln start: 0 end: (BufFrames bufnum: bufnum) - 1 dur: dur.
    ^BufRd numChan: 1 bufnum: bufnum phase: phase loop: 0 interpolation: 2!
primaryFactoryMethod
    ^#bufnum:dur:! !
