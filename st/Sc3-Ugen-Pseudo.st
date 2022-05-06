PseudoUgen subclass: #ADSR
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ADSR comment: 'EnvGen of simplified EnvADSR'!

!ADSR class methodsFor: 'instance creation'!
gate: g attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 curve: p5
    ^(EnvADSR attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: 1 curve: p5) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:decayTime:sustainLevel:releaseTime:curve:!
!

PseudoUgen subclass: #ASR
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ASR comment: 'EnvGen of simplified EnvASR'!

!ASR class methodsFor: 'instance creation'!
gate: g attackTime: p1 releaseTime: p2 curve: p3
    ^(EnvASR attackTime: p1 sustainLevel: 1 releaseTime: p2 curve: p3) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:releaseTime:curve:!
!

PseudoUgen subclass: #AudioIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

AudioIn comment: 'Audio input'!

!AudioIn class methodsFor: 'instance creation'!
channels: channels
    ^In numChan: 1 bus: NumOutputBuses new + channels - 1!
primaryFactoryMethod
    ^#channels:!
!

PseudoUgen subclass: #BufAlloc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufAlloc comment: 'Allocate a buffer local to the synth.'!

!BufAlloc class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames
    ^LocalBuf numChannels: numChannels numFrames: numFrames!
primaryFactoryMethod
    ^#numChannels:numFrames:!
!

PseudoUgen subclass: #BufRec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufRec comment: 'Record signal into a Buffer'!

!BufRec class methodsFor: 'instance creation'!
bufnum: bufnum reset: reset inputArray: inputArray
    ^RecordBuf bufnum: bufnum offset: 0 recLevel: 1 preLevel: 0 run: 1 loop: 1 trigger: reset doneAction: 0 inputArray: inputArray!
primaryFactoryMethod
    ^#bufnum:reset:inputArray:!
!

PseudoUgen subclass: #Cc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Cc comment: 'Continuous controller input'!

!Cc class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 11000 + index) kr!
primaryFactoryMethod
    ^#index:!
!

PseudoUgen subclass: #Changed
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Changed comment: 'Triggers when a value changes'!

!Changed class methodsFor: 'instance creation'!
input: input threshold: threshold
    ^(HPZ1 in: input) abs > threshold!
primaryFactoryMethod
    ^#input:threshold:!
!

PseudoUgen subclass: #Choose
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Choose comment: 'Random sequence generator.'!

!Choose class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Drand repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!

PseudoUgen subclass: #ControlIn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ControlIn comment: 'Control input'!

!ControlIn class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^(In numChan: numChan bus: bus) kr!
channels: channels
    ^In numChan: 1 bus: channels - 1!
primaryFactoryMethod
    ^#channels:!
!

PseudoUgen subclass: #Cutoff
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Cutoff comment: 'EnvGen of simple cutoff envelope'!

!Cutoff class methodsFor: 'instance creation'!
sustainTime: p1 releaseTime: p2 curve: p3
    ^(EnvSpec levels: #(1 1 0) times: {p1. p2} curves: p3 releaseNode: nil loopNode: nil offset: 0)  asEnvGen
!
primaryFactoryMethod
    ^#sustainTime:releaseTime:curve:!
!

PseudoUgen subclass: #DmdFor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DmdFor comment: 'Demand with duration input (Duty).'!

!DmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^Duty dur: dur reset: reset doneAction: 0 level: level
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!

PseudoUgen subclass: #DmdOn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DmdOn comment: 'Demand with trigger input (Demand).'!

!DmdOn class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens
    ^Demand trig: trig reset: reset demandUGens: demandUGens!
primaryFactoryMethod
    ^#trig:reset:demandUGens:!
!
PseudoUgen subclass: #DynKlank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

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
    ^#input:freqscale:freqoffset:decayscale:specificationsArrayRef:!
!

PseudoUgen subclass: #InFb
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

InFb comment: 'Read signal from a bus with a current or one cycle old timestamp.'!

!InFb class methodsFor: 'instance creation'!
numChan: numChan bus: bus
    ^InFeedback numChan: numChan bus: bus!
primaryFactoryMethod
    ^#numChan:bus:!
!

PseudoUgen subclass: #KeyDown
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyDown comment: 'No comment'!

!KeyDown class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #KeyPitch
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyPitch comment: 'No comment'!

!KeyPitch class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 7 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #KeyTimbre
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyTimbre comment: 'No comment'!

!KeyTimbre class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #KeyVelocity
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KeyVelocity comment: 'No comment'!

!KeyVelocity class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #LinLin
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinLin comment: 'Map a linear range to a linear range'!

!LinLin class methodsFor: 'instance creation'!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    ^(in - srclo) / (srchi - srclo) * (dsthi - dstlo) + dstlo!
primaryFactoryMethod
    ^#in:srclo:srchi:dstlo:dsthi:!
!

PseudoUgen subclass: #LinSeg
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinSeg comment: 'Line Segment Envelope'!

!LinSeg class methodsFor: 'instance creation'!
gate: g coord: c
    | l t |
    l := (1 to: c size by: 2) collect: [ :i | c at: i ].
    t := (2 to: c size by: 2) collect: [ :i | c at: i ].
    ^(EnvSpec levels: l times: t curves: #(#lin)) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:coord:!
!

PseudoUgen subclass: #Ln
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ln comment: 'Line generator'!

!Ln class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^Line start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:!
!

PseudoUgen subclass: #Osc1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Osc1 comment: 'An oscillator that reads through a table once.'!

!Osc1 class methodsFor: 'instance creation'!
bufnum: bufnum dur: dur
    | phase |
    phase := Ln start: 0 end: (BufFrames bufnum: bufnum) - 1 dur: dur.
    ^BufRd numChan: 1 bufnum: bufnum phase: phase loop: 0 interpolation: 2!
primaryFactoryMethod
    ^#bufnum:dur:!
!
PseudoUgen subclass: #PMOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PMOsc comment: 'Phase modulation sine oscillator pair.'!

!PMOsc class methodsFor: 'instance creation'!
carfreq: cf modfreq: mf pmindex: pm modphase: mp
    ^SinOsc freq: cf phase: (SinOsc freq: mf phase: mp) * pm!
carfreq: cf modfreq: mf pmindex: pm modphase: mp mul: mul
    ^(self carfreq: cf modfreq: mf pmindex: pm modphase: mp) * mul!
primaryFactoryMethod
    ^#carfreq:modfreq:pmindex:modphase:!
!

PseudoUgen subclass: #PenAngle
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenAngle comment: 'No comment'!

!PenAngle class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 4 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #PenDown
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenDown comment: 'No comment'!

!PenDown class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #PenRadius
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenRadius comment: 'No comment'!

!PenRadius class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 5 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #PenX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenX comment: 'No comment'!

!PenX class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 1 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #PenY
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenY comment: 'No comment'!

!PenY class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #PenZ
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PenZ comment: 'No comment'!

!PenZ class methodsFor: 'instance creation'!
voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
primaryFactoryMethod ^#voiceNumber:!
!

PseudoUgen subclass: #RingzBank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RingzBank comment: 'Parallel bank of Ringz'!

!RingzBank class methodsFor: 'instance creation'!
input: input freq: freq amp: amp time: time
    ^Klank input: input freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: time)!
primaryFactoryMethod
    ^#input:freq:amp:time:!
!

PseudoUgen subclass: #Select2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Select2 comment: 'Select one of two branches based on predicate signal'!

!Select2 class methodsFor: 'instance creation'!
predicate: predicate ifTrue: trueUgen ifFalse: falseUgen
    ^(predicate * (trueUgen - falseUgen)) + falseUgen!
primaryFactoryMethod
    ^#predicate:ifTrue:ifFalse:!
!

PseudoUgen subclass: #SelectX
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SelectX comment: 'Mix one output from many sources'!

!SelectX class methodsFor: 'instance creation'!
which: which array: array
    ^XFade2
        inA: (Select which: (which roundTo: 2) array: array)
        inB: (Select which: (which truncateTo: 2) + 1 array: array)
        pan: ((which * 2 - 1) fold2: 1)
        level: 1!
primaryFactoryMethod
    ^#which:array:!
!
PseudoUgen subclass: #Seq
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Seq comment: 'Sequence generator.'!

!Seq class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dseq repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
PseudoUgen subclass: #Ser
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Ser comment: 'Demand rate arithmetic series Ugen.'!

!Ser class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dser repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
PseudoUgen subclass: #Shuf
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Shuf comment: 'Demand rate random sequence generator'!

!Shuf class methodsFor: 'instance creation'!
repeats: repeats list: list
    ^Dshuf repeats: repeats list: list!
primaryFactoryMethod
    ^#repeats:list:!
!
PseudoUgen subclass: #SinOscBank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOscBank comment: 'Parallel bank of SinOsc'!

!SinOscBank class methodsFor: 'instance creation'!
freq: freq amp: amp phase: phase
    ^Klang freqscale: 1 freqoffset: 0 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: phase)!
primaryFactoryMethod
    ^#freq:amp:phase:!
!

PseudoUgen subclass: #Splay
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Splay comment: 'Pan a set of channels across the stereo field.'!

!Splay class methodsFor: 'as yet unclassified'!
inArray: inArray spread: spread level: level center: center levelComp: levelComp
    | n n1 pos lvl |
    n := 2 max: (inArray size).
    n1 := n - 1.
    pos := ((0 to: n1) * (2 / n1) - 1) * spread + center.
    lvl := levelComp ifTrue: [ level * n reciprocal sqrt ] ifFalse: [ lvl ].
    ^(Pan2 in: inArray pos: pos level: level) sum!
primaryFactoryMethod ^#'inArray:spread:level:center:levelComp:'!
!

PseudoUgen subclass: #Splay2
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Splay2 comment: 'Pan an array of channels across the stereo field.'!

!Splay2 class methodsFor: 'instance creation'!
inArray: inArray
    ^Splay inArray: inArray spread: 1 level: 1 center: 0 levelComp: true!
primaryFactoryMethod
    ^#inArray:!
!
PseudoUgen subclass: #Sw
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sw comment: 'Switch (maintained or momentary) controller input'!

!Sw class methodsFor: 'instance creation'!
index: index
    ^(In numChan: 1 bus: 12000 + index) kr!
primaryFactoryMethod
    ^#index:!
!

PseudoUgen subclass: #TChoose
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TChoose comment: 'Randomly select one of several inputs on trigger.'!

!TChoose class methodsFor: 'instance creation'!
trig: trig array: array
    ^Select which: (TIRand lo: 0 hi: (array size - 1) trig: trig) array: array!
primaryFactoryMethod
    ^#trig:array:!
!

PseudoUgen subclass: #TDmdFor
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TDmdFor comment: 'Demand with duration input (Duty).'!

!TDmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^TDuty dur: dur reset: reset doneAction: 0 level: level gapFirst: 0
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!

PseudoUgen subclass: #TLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TLine comment: 'Triggered Line.'!

!TLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: (Array with: #lin with: #lin)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:!
!

PseudoUgen subclass: #TXLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TXLine comment: 'Triggered XLine.'!

!TXLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: #(#exp #exp)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:!
!

PseudoUgen subclass: #Tr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Tr comment: 'Trig with duration of one sample.'!

!Tr class methodsFor: 'instance creation'!
in: in
    ^Trig in: in dur: SampleDur new "Should be SampleDur if audio rate and ControlDur if control rate, requires QuantaDur or some such."!
primaryFactoryMethod
    ^#in:!
!

PseudoUgen subclass: #Tr1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Tr1 comment: 'Trig1 with duration of one sample.'!

!Tr1 class methodsFor: 'instance creation'!
in: in
    ^Trig1 in: in dur: SampleDur new!
primaryFactoryMethod
    ^#in:!
!

PseudoUgen subclass: #XLn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

XLn comment: 'Line generator'!

!XLn class methodsFor: 'instance creation'!
start: start end: end dur: dur
    ^XLine start: start end: end dur: dur doneAction: 0!
primaryFactoryMethod
    ^#start:end:dur:!
!
