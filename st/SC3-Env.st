Object subclass: #Env
  instanceVariableNames: 'levels times curves releaseNode loopNode offset'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Env comment: 'Specification for a segmented envelope'!

!Env methodsFor: 'accessing'!
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

!Env methodsFor: 'naming'!
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

!Env class methodsFor: 'instance creation'!
  levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
    | u |
    u := self new.
    u levels: p1.
    u times: p2.
    u curves: p3.
    u releaseNode: p4.
    u loopNode: p5.
    u offset: p6.
    ^u
  !
  levels: p1 times: p2 curves: p3
    ^Env levels: p1 times: p2 curves: p3 releaseNode: nil loopNode: nil offset: 0
  !
!

!Env methodsFor: 'envelope coordinates'!
  coord
    | n r d |
    n := times size.
    r := Array new: ((n + 1) * 4).
    r at: 1 put: (levels at: 1).
    r at: 2 put: n.
    r at: 3 put: (releaseNode ifNil: [-99] ifNotNil: [releaseNode]).
    r at: 4 put: (loopNode ifNil: [-99] ifNotNil: [loopNode]).
    d := self curveNameDictionary.
    (1 to: n) do: [: i |
      r at: (1 + (i * 4)) put: (levels at: (i + 1)).
      r at: (2 + (i * 4)) put: (times at: i).
      r at: (3 + (i * 4)) put: (d at: (curves at: i) ifAbsent: [5]).
      r at: (4 + (i * 4)) put: ((curves at: i) isSymbol ifTrue: [0] ifFalse: [curves at: i]) ].
    ^r
  !
!

!Env methodsFor: 'envelope generator'!
  gen: g doneAction: d
    ^EnvGen gate: g levelScale: 1 levelBias: 0 timeScale: 1 doneAction: d envelope: self coord
  !
  gen: g
    ^self gen: g doneAction: 0
  !
  gen
    ^self gen: 1
  !
!

Object subclass: #EnvBuilder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvBuilder comment: 'Envelope types'!

!EnvBuilder methodsFor: 'envelope coordinates'!
  env
    "Make Env instance"
    self subclassResponsibility
  !
  envGen: g doneAction: d
    "Make EnvGen instance with indicated gate and doneAction"
    ^(self env) gen: g doneAction: d
  !
  envGen: g
    "Make EnvGen instance with indicated gate and default doneAction (0=doNothing)"
    ^self envGen: g doneAction: 0
  !
  envGen
    "Make EnvGen instance with default gate (1) and doneAction (0=doNothing)"
    ^self envGen: 1 doneAction: 0
  !
!

!EnvBuilder class methodsFor: 'instance creation'!
  apply: arg
    ^self perform: (self primaryFactoryMethod) withArguments: arg
  !
!

EnvBuilder subclass: #EnvPerc
  instanceVariableNames: 'attackTime releaseTime level curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvPerc comment: 'Attack-Release Envelope (Fixed Duration, No Gate)'!

!EnvPerc methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curves ^curves !
  curves: x curves := x. !
  level ^level !
  level: x level := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
!

!EnvPerc class methodsFor: 'instance creation'!
  attackTime: p1 releaseTime: p2 level: p3 curves: p4
    | u |
    u := self new.
    u attackTime: p1.
    u releaseTime: p2.
    u level: p3.
    u curves: p4.
    ^u
  !
!

!EnvPerc methodsFor: 'envelope coordinates'!
  env
    ^Env
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curves
      releaseNode: nil
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvLinen
  instanceVariableNames: 'attackTime sustainTime releaseTime level curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvLinen comment: 'Trapezoidal Envelope (Fixed Duration, No Gate)'!

!EnvLinen methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curves ^curves !
  curves: x curves := x. !
  level ^level !
  level: x level := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
  sustainTime ^sustainTime !
  sustainTime: x sustainTime := x. !
!

!EnvLinen class methodsFor: 'instance creation'!
  attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curves: p5
    | u |
    u := self new.
    u attackTime: p1.
    u sustainTime: p2.
    u releaseTime: p3.
    u level: p4.
    u curves: p5.
    ^u
  !
  primaryFactoryMethod ^#attackTime:sustainTime:releaseTime:level:curves:!
!

!EnvLinen methodsFor: 'envelope coordinates'!
  env
    ^Env
      levels: (Array with: 0 with: level with: level with: 0)
      times: (Array with: attackTime with: sustainTime with: releaseTime)
      curves: curves
      releaseNode: nil
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvASR
  instanceVariableNames: 'attackTime sustainLevel releaseTime curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvASR comment: 'Attack-Sustain-Release Envelope'!

!EnvASR methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curves ^curves !
  curves: x curves := x. !
  releaseTime ^releaseTime !
  releaseTime: x releaseTime := x. !
  sustainLevel ^sustainLevel !
  sustainLevel: x sustainLevel := x. !
!

!EnvASR class methodsFor: 'instance creation'!
  attackTime: p1 sustainLevel: p2 releaseTime: p3 curves: p4
    | u |
    u := self new.
    u attackTime: p1.
    u sustainLevel: p2.
    u releaseTime: p3.
    u curves: p4.
    ^u
  !
!

!EnvASR methodsFor: 'envelope coordinates'!
  env
    ^Env
      levels: (Array with: 0 with: sustainLevel with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curves
      releaseNode: 1
      loopNode: nil
      offset: 0
  !
!

EnvBuilder subclass: #EnvADSR
  instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EnvADSR comment: 'Attack-Decay-Sustain-Release Envelope'!

!EnvADSR methodsFor: 'accessing'!
  attackTime ^attackTime !
  attackTime: x attackTime := x. !
  curves ^curves !
  curves: x curves := x. !
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
  attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curves: p6
    | u |
    u := self new.
    u attackTime: p1.
    u decayTime: p2.
    u sustainLevel: p3.
    u releaseTime: p4.
    u peakLevel: p5.
    u curves: p6.
    ^u
  !
!

!EnvADSR methodsFor: 'envelope coordinates'!
  env
    ^Env
      levels: (Array with: 0 with: peakLevel with: peakLevel * sustainLevel with: 0)
      times: (Array with: attackTime with: decayTime with: releaseTime)
      curves: curves
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

EnvSine comment: 'Sine (Hanning Window) Envelope (Fixed Duration, No Gate)'!

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
!

!EnvSine methodsFor: 'envelope coordinates'!
  env
    ^Env
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: dur / 2 with: dur / 2)
      curves: #(3 3)
      releaseNode: nil
      loopNode: nil
      offset: 0
  !
!
