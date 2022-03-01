Object subclass: #EnvSpec
  instanceVariableNames: 'levels times curves releaseNode loopNode offset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvSpec comment: 'Specification for a segmented envelope'!

!EnvSpec methodsFor: 'accessing'!
  curves ^curves !
  curves: x curves := x. !
  levels ^levels !
  levels: x levels := x. !
  loopNode ^loopNode !
  loopNode: x loopNode := x. !
  offset ^offset !
  offset: x offset := x. !
  releaseNode ^releaseNode !
  releaseNode: x releaseNode := x. !
  times ^times !
  times: x times := x. !
!

!EnvSpec methodsFor: 'naming'!
  curveNameDictionary
    ^Dictionary newFrom: {
      #step -> 0.
      #lin -> 1. #linear -> 1.
      #exp -> 2. #exponential -> 2.
      #sin -> 3. #sine -> 3.
      #wel -> 4. #welch -> 4.
      #sqr -> 6. #squared -> 6.
      #cub -> 7. #cubed -> 7.
      #hold -> 8 }
  !
!

!EnvSpec class methodsFor: 'instance creation'!
  levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
    "Allow curves to be a scalar"
    | u |
    u := self new.
    u levels: p1.
    u times: p2.
    u curves: ((p3 isSymbol or: p3 isNumber) ifTrue: [ Array with: p3 ] ifFalse: [ p3 ]).
    u releaseNode: p4.
    u loopNode: p5.
    u offset: p6.
    ^u
  !
  levels: p1 times: p2 curves: p3
    ^EnvSpec levels: p1 times: p2 curves: p3 releaseNode: nil loopNode: nil offset: 0
  !
  levels: p1 times: p2
    ^EnvSpec levels: p1 times: p2 curves: #(1)
  !
  primaryFactoryMethod
    ^#levels:times:curves:releaseNode:loopNode:offset:
  !
  apply: arg
    ^self perform: (self primaryFactoryMethod) withArguments: arg
  !
!

!EnvSpec methodsFor: 'envelope coordinates'!
  coord
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
!

!EnvSpec methodsFor: 'envelope generator'!
  asEnvGen: g withDoneAction: d
    "Make EnvGen instance with indicated gate and doneAction"
    ^EnvGen gate: g levelScale: 1 levelBias: 0 timeScale: 1 doneAction: d envelope: self coord
  !
  asEnvGen: g
    "Make EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self asEnvGen: g withDoneAction: 0
  !
  asEnvGen
    "Make EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self asEnvGen: 1
  !
!

EnvSpec subclass: #Env
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Env comment: 'Alias for EnvSpec'!

Object subclass: #EnvBuilder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvBuilder comment: 'Envelope types'!

!EnvBuilder class methodsFor: 'instance creation'!
  apply: arg
    "EnvBuilder classes have a primaryFactoryMethod"
    ^self perform: (self primaryFactoryMethod) withArguments: arg
  !
!

!EnvBuilder methodsFor: 'envelope specification'!
  asEnvSpec
    "EnvBuilder instances can generate EnvSpec instances"
    self subclassResponsibility
  !
  asEnvGen: g withDoneAction: d
    "Make EnvGen instance with indicated gate and doneAction"
    ^(self asEnvSpec) asEnvGen: g withDoneAction: d
  !
  asEnvGen: g
    "Make EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self asEnvGen: g withDoneAction: 0
  !
  asEnvGen
    "Make EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self asEnvGen: 1 withDoneAction: 0
  !
!

EnvBuilder subclass: #EnvPerc
  instanceVariableNames: 'attackTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvPerc comment: 'Attack-Release Envelope (Fixed duration, no release node)'!

!EnvPerc methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curve ^curve !
  curve: x curve := x. !
  level ^level !
  level: x level := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
!

!EnvPerc class methodsFor: 'instance creation'!
  attackTime: p1 releaseTime: p2 level: p3 curve: p4
    | u |
    u := self new.
    u attackTime: p1.
    u releaseTime: p2.
    u level: p3.
    u curve: p4.
    ^u
  !
  primaryFactoryMethod ^#attackTime:releaseTime:level:curve:!
!

!EnvPerc methodsFor: 'envelope specification'!
  asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvLinen
  instanceVariableNames: 'attackTime sustainTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvLinen comment: 'Trapezoidal Envelope (Fixed duration, no release node)'!

!EnvLinen methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curve ^curve !
  curve: x curve := x. !
  level ^level !
  level: x level := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
  sustainTime ^sustainTime !
  sustainTime: x sustainTime := x. !
!

!EnvLinen class methodsFor: 'instance creation'!
  attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
    | u |
    u := self new.
    u attackTime: p1.
    u sustainTime: p2.
    u releaseTime: p3.
    u level: p4.
    u curve: p5.
    ^u
  !
  primaryFactoryMethod ^#attackTime:sustainTime:releaseTime:level:curve:!
!

!EnvLinen methodsFor: 'envelope specification'!
  asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: level with: 0)
      times: (Array with: attackTime with: sustainTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvASR
  instanceVariableNames: 'attackTime sustainLevel releaseTime curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvASR comment: 'Attack-Sustain-Release Envelope'!

!EnvASR methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curve ^curve !
  curve: x curve := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
  sustainLevel ^sustainLevel !
  sustainLevel: x sustainLevel := x. !
!

!EnvASR class methodsFor: 'instance creation'!
  attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
    | u |
    u := self new.
    u attackTime: p1.
    u sustainLevel: p2.
    u releaseTime: p3.
    u curve: p4.
    ^u
  !
!

!EnvASR methodsFor: 'envelope specification'!
  asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: sustainLevel with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: 1
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvADSR
  instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvADSR comment: 'Attack-Decay-Sustain-Release Envelope'!

!EnvADSR methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curve ^curve !
  curve: x curve := x. !
  decayTime ^decayTime !
  decayTime: x decayTime := x. !
  peakLevel ^peakLevel !
  peakLevel: x peakLevel := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
  sustainLevel ^sustainLevel !
  sustainLevel: x sustainLevel := x. !
!

!EnvADSR class methodsFor: 'instance creation'!
  attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
    | u |
    u := self new.
    u attackTime: p1.
    u decayTime: p2.
    u sustainLevel: p3.
    u releaseTime: p4.
    u peakLevel: p5.
    u curve: p6.
    ^u
  !
  primaryFactoryMethod ^#attackTime:decayTime:sustainLevel:releaseTime:peakLevel:curve:!
!

!EnvADSR methodsFor: 'envelope specification'!
  asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: peakLevel with: peakLevel * sustainLevel with: 0)
      times: (Array with: attackTime with: decayTime with: releaseTime)
      curves: curve
      releaseNode: 2
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvSine
  instanceVariableNames: 'dur level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvSine comment: 'Sine (Hanning Window) Envelope Builder (Fixed duration, no release node)'!

!EnvSine methodsFor: 'accessing'!
  dur ^dur !
  dur: x dur := x. !
  level ^level !
  level: x level := x. !
!

!EnvSine class methodsFor: 'instance creation'!
  dur: p1 level: p2
    | u |
    u := self new.
    u dur: p1.
    u level: p2.
    ^u
  !
  primaryFactoryMethod ^#dur:level:!
!

!EnvSine methodsFor: 'envelope specification'!
  asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: dur / 2 with: dur / 2)
      curves: #(3 3)
      releaseNode: nil
      loopNode: 0
      offset: 0
  !
!

AbstractUGen subclass: #ADSR instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

ADSR comment: 'EnvGen of simplified EnvADSR'!

!ADSR class methodsFor: 'instance creation'!
  gate: g attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 curve: p5
    ^(EnvADSR attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: 1 curve: p5) asEnvGen: g
  !
  primaryFactoryMethod ^#gate:attackTime:decayTime:sustainLevel:releaseTime:curve:!
!

AbstractUGen subclass: #ASR instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

ASR comment: 'EnvGen of simplified EnvASR'!

!ASR class methodsFor: 'instance creation'!
  gate: g attackTime: p1 releaseTime: p2 curve: p3
    ^(EnvASR attackTime: p1 sustainLevel: 1 releaseTime: p2 curve: p3) asEnvGen: g
  !
  primaryFactoryMethod ^#gate:attackTime:releaseTime:curve:!
!

AbstractUGen subclass: #LinSeg instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

LinSeg comment: 'Line Segment Envelope'!

!LinSeg class methodsFor: 'instance creation'!
  gate: g coord: c
    | l t |
    l := (1 to: c size by: 2) collect: [ :i | c at: i ].
    t := (2 to: c size by: 2) collect: [ :i | c at: i ].
    ^(EnvSpec levels: l times: t curves: #(#lin)) asEnvGen: g
  !
  primaryFactoryMethod ^#gate:coord:!
!

AbstractUGen subclass: #Cutoff instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!

Cutoff comment: 'EnvGen of simple cutoff envelope'!

!Cutoff class methodsFor: 'instance creation'!
  sustainTime: p1 releaseTime: p2 curve: p3
    ^(EnvSpec levels: #(1 1 0) times: {p1. p2} curves: p3 releaseNode: nil loopNode: nil offset: 0)  asEnvGen
  !
  primaryFactoryMethod ^#sustainTime:releaseTime:curve:!
!
