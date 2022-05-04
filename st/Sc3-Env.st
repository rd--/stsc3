Object subclass: #EnvSpec
  instanceVariableNames: 'levels times curves releaseNode loopNode offset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvSpec comment: 'Specification for a segmented envelope'!

!EnvSpec methodsFor: 'naming'!
curveNameDictionary
    "Dictionary mapping curve names to their integers codes."
    ^Dictionary newFrom: {
      #step -> 0.
      #lin -> 1. #linear -> 1.
      #exp -> 2. #exponential -> 2.
      #sin -> 3. #sine -> 3.
      #wel -> 4. #welch -> 4.
      #sqr -> 6. #squared -> 6.
      #cub -> 7. #cubed -> 7.
      #hold -> 8 }! !

!EnvSpec methodsFor: 'initialisation'!
levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
    "Allow curves to be a scalar"
    levels := p1.
    times := p2.
    curves := (p3 isSymbol or: p3 isNumber) ifTrue: [ Array with: p3 ] ifFalse: [ p3 ].
    releaseNode := p4.
    loopNode := p5.
    offset := p6! !

!EnvSpec class methodsFor: 'instance creation'!
levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
    ^self new levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6!
levels: p1 times: p2 curves: p3
    "Default values for releaseNode: and loopNode: and offset."
    ^self levels: p1 times: p2 curves: p3 releaseNode: nil loopNode: nil offset: 0!
levels: p1 times: p2
    "Default values for curves: and releaseNode: and loopNode: and offset."
    ^self levels: p1 times: p2 curves: #(1)!
primaryFactoryMethod
    ^#levels:times:curves:releaseNode:loopNode:offset:!
apply: arg
    "EnvSpec has a primary factory method."
    ^self perform: (self primaryFactoryMethod) withArguments: arg! !

!EnvSpec methodsFor: 'envelope coordinates'!
coordinateArrayForEnvGen
    "My linear co-ordinate array, as required by EnvGen."
    | n r d |
    n := levels size - 1.
    r := Array new: ((n + 1) * 4).
    r at: 1 put: (levels at: 1).
    r at: 2 put: n.
    r at: 3 put: (releaseNode ifNil: [ -99 ] ifNotNil: [ releaseNode ]).
    r at: 4 put: (loopNode ifNil: [ -99 ] ifNotNil: [ loopNode ]).
    d := self curveNameDictionary.
    (1 to: n) do: [ :i |
      | c |
      c := curves atWrap: i.
      r at: (1 + (i * 4)) put: (levels at: (i + 1)).
      r at: (2 + (i * 4)) put: (times atWrap: i).
      r at: (3 + (i * 4)) put: (d at: c ifAbsent: [ 5 ]).
      r at: (4 + (i * 4)) put: (c isSymbol ifTrue: [ 0 ] ifFalse: [ c ]) ].
    ^r
!
asArray
    "Convenience"
    ^self coordinateArrayForEnvGen! !

!EnvSpec methodsFor: 'envelope generator'!
asEnvGen: g withDoneAction: d
    "Make EnvGen instance with indicated gate and doneAction"
    ^EnvGen gate: g levelScale: 1 levelBias: 0 timeScale: 1 doneAction: d envelope: self coordinateArrayForEnvGen
!
asEnvGen: g
    "Make EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self asEnvGen: g withDoneAction: 0
!
asEnvGen
    "Make EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self asEnvGen: 1! !

EnvSpec subclass: #Env
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Env comment: 'Alias for EnvSpec'!

Object subclass: #EnvBuilder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

EnvBuilder comment: 'Envelope types'!

!EnvBuilder class methodsFor: 'instance creation'!
apply: arg
    "EnvBuilder classes have a primaryFactoryMethod"
    ^self perform: (self primaryFactoryMethod) withArguments: arg! !

!EnvBuilder methodsFor: 'envelope specification'!
asEnvSpec
    "Myself as an EnvSpec"
    self subclassResponsibility
!
asArray
    "Myself as a coordinary array suitable for EnvGen "
    ^self asEnvSpec coordinateArrayForEnvGen
!
asEnvGen: g withDoneAction: d
    "Myself as an EnvGen instance with indicated gate and doneAction"
    ^(self asEnvSpec) asEnvGen: g withDoneAction: d
!
asEnvGen: g
    "Myself as an EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self asEnvGen: g withDoneAction: 0
!
asEnvGen
    "Myself as an EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self asEnvGen: 1 withDoneAction: 0! !

EnvBuilder subclass: #EnvPerc
  instanceVariableNames: 'attackTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

EnvPerc comment: 'Attack-Release Envelope (Fixed duration, no release node)'!

!EnvPerc methodsFor: 'initialisation'!
attackTime: p1 releaseTime: p2 level: p3 curve: p4
    attackTime := p1.
    releaseTime := p2.
    level := p3.
    curve := p4! !

!EnvPerc class methodsFor: 'instance creation'!
attackTime: p1 releaseTime: p2 level: p3 curve: p4
    ^ self new attackTime: p1 releaseTime: p2 level: p3 curve: p4
!
primaryFactoryMethod
     ^#attackTime:releaseTime:level:curve:! !

!EnvPerc methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0! !

EnvBuilder subclass: #EnvLinen
  instanceVariableNames: 'attackTime sustainTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

EnvLinen comment: 'Trapezoidal Envelope (Fixed duration, no release node)'!

!EnvLinen methodsFor: 'initialisation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
    attackTime := p1.
    sustainTime := p2.
    releaseTime := p3.
    level := p4.
    curve := p5! !

!EnvLinen class methodsFor: 'instance creation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
    ^self new attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
!
primaryFactoryMethod
    ^#attackTime:sustainTime:releaseTime:level:curve:! !

!EnvLinen methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: level with: 0)
      times: (Array with: attackTime with: sustainTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0! !

EnvBuilder subclass: #EnvASR
  instanceVariableNames: 'attackTime sustainLevel releaseTime curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

EnvASR comment: 'Attack-Sustain-Release Envelope'!

!EnvASR methodsFor: 'initialisation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
    attackTime := p1.
    sustainLevel := p2.
    releaseTime := p3.
    curve := p4! !

!EnvASR class methodsFor: 'instance creation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
    ^self new attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
!
primaryFactoryMethod
    ^#attackTime:sustainLevel:releaseTime:curve:! !

!EnvASR methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: sustainLevel with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: 1
      loopNode: nil
      offset: 0! !

EnvBuilder subclass: #EnvADSR
  instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

EnvADSR comment: 'Attack-Decay-Sustain-Release Envelope'!

!EnvADSR methodsFor: 'initialisation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
    attackTime := p1.
    decayTime := p2.
    sustainLevel := p3.
    releaseTime := p4.
    peakLevel := p5.
    curve := p6! !

!EnvADSR class methodsFor: 'instance creation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
    ^self new attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
!
primaryFactoryMethod
    ^#attackTime:decayTime:sustainLevel:releaseTime:peakLevel:curve:! !

!EnvADSR methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: peakLevel with: peakLevel * sustainLevel with: 0)
      times: (Array with: attackTime with: decayTime with: releaseTime)
      curves: curve
      releaseNode: 2
      loopNode: nil
      offset: 0! !

EnvBuilder subclass: #EnvSine
  instanceVariableNames: 'dur level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvSine comment: 'Sine (Hanning Window) Envelope Builder (Fixed duration, no release node)'!

!EnvSine methodsFor: 'initialisation'!
dur: p1 level: p2
    dur := p1.
    level := p2! !

!EnvSine class methodsFor: 'instance creation'!
dur: p1 level: p2
    ^self new dur: p1 level: p2!
primaryFactoryMethod
    ^#dur:level:! !

!EnvSine methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: dur / 2 with: dur / 2)
      curves: #(3 3)
      releaseNode: nil
      loopNode: 0
      offset: 0! !

AbstractUgen subclass: #ADSR instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

ADSR comment: 'EnvGen of simplified EnvADSR'!

!ADSR class methodsFor: 'instance creation'!
gate: g attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 curve: p5
    ^(EnvADSR attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: 1 curve: p5) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:decayTime:sustainLevel:releaseTime:curve:! !

AbstractUgen subclass: #ASR instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

ASR comment: 'EnvGen of simplified EnvASR'!

!ASR class methodsFor: 'instance creation'!
gate: g attackTime: p1 releaseTime: p2 curve: p3
    ^(EnvASR attackTime: p1 sustainLevel: 1 releaseTime: p2 curve: p3) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:releaseTime:curve:! !

AbstractUgen subclass: #LinSeg instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

LinSeg comment: 'Line Segment Envelope'!

!LinSeg class methodsFor: 'instance creation'!
gate: g coord: c
    | l t |
    l := (1 to: c size by: 2) collect: [ :i | c at: i ].
    t := (2 to: c size by: 2) collect: [ :i | c at: i ].
    ^(EnvSpec levels: l times: t curves: #(#lin)) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:coord:! !

AbstractUgen subclass: #Cutoff instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Cutoff comment: 'EnvGen of simple cutoff envelope'!

!Cutoff class methodsFor: 'instance creation'!
sustainTime: p1 releaseTime: p2 curve: p3
    ^(EnvSpec levels: #(1 1 0) times: {p1. p2} curves: p3 releaseNode: nil loopNode: nil offset: 0)  asEnvGen
!
primaryFactoryMethod
    ^#sustainTime:releaseTime:curve:! !
