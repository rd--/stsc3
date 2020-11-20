Object subclass: #Envelope
 instanceVariableNames: 'levels times curves releaseNode loopNode offset'
 classVariableNames: 'CurveNameDictionary'
 poolDictionaries: ''
 category: 'SC3'
!
!Envelope commentStamp: '<historical>' prior: 0! Specification for a segmented envelope!

!Envelope methodsFor: 'accessing'!
 curves ^ curves !
 curves: x curves := x. !
 levels ^ levels !
 levels: x levels := x. !
 loopNode ^ loopNode !
 loopNode: x loopNode := x. !
 offset ^ offset !
 offset: x offset := x. !
 releaseNode ^ releaseNode !
 releaseNode: x releaseNode := x. !
 times ^ times !
 times: x times := x. !
!

!Envelope class methodsFor: 'accessing'!
 curveNameDictionary ^ CurveNameDictionary !
!

!Envelope class methodsFor: 'instance creation'!
 levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
 |u| u := self new . u levels: p1 . u times: p2 . u curves: p3 . u releaseNode: p4 . u loopNode: p5 . u offset: p6 .
 ^ u !
!

!Envelope class methodsFor: 'class initialization'!
 initialize
 CurveNameDictionary :=
 Dictionary newFrom: {
  #step -> 0 .
  #lin -> 1 . #linear -> 1 .
  #exp -> 2 . #exponential -> 2 .
  #sin -> 3 . #sine -> 3 .
  #wel -> 4 . #welch -> 4 .
  #sqr -> 6 . #squared -> 6 .
  #cub -> 7 . #cubed -> 7 .
  #hold -> 8 } !
!

!Envelope methodsFor: 'envelope coordinates'!
 coord
 |r n|
 n := times size .
 r := Array new: ((n + 1) * 4) .
 r at: 1 put: (levels at: 1) .
 r at: 2 put: n .
 r at: 3 put: (releaseNode ifNil: -99 ifNotNil: releaseNode) .
 r at: 4 put: (loopNode ifNil: -99 ifNotNil: loopNode) .
 (1 to: n) do: [: i|
  r at: (1 + (i * 4)) put: (levels at: (i + 1)) .
  r at: (2 + (i * 4)) put: (times at: i) .
  r at: (3 + (i * 4)) put: (self class curveNameDictionary at: (curves at: i) ifAbsent: 5) .
  r at: (4 + (i * 4)) put: ((curves at: i) isSymbol ifTrue: 0 ifFalse: (curves at: i)) ] .
 ^r !
!

!Envelope methodsFor: 'envelope generator'!
 gen: x ^ EnvGen gate: 1 levelScale: 1 levelBias: 0 timeScale: 1 doneAction: x envelope: self coord !
 gen ^ self gen: 0 !
!

Object subclass: #EnvPerc
 instanceVariableNames: 'attackTime releaseTime level curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!EnvPerc commentStamp: '<historical>' prior: 0! Attack-Release Envelope!

!EnvPerc methodsFor: 'accessing'!
 attackTime ^ attackTime !
 attackTime: x attackTime := x. !
 curves ^ curves !
 curves: x curves := x. !
 level ^ level !
 level: x level := x. !
 releaseTime ^ releaseTime !
 releaseTime: x releaseTime := x. !
!

!EnvPerc class methodsFor: 'instance creation'!
 attackTime: p1 releaseTime: p2 level: p3 curves: p4
 |u| u := self new . u attackTime: p1 . u releaseTime: p2 . u level: p3 . u curves: p4 . ^ u !
!

!EnvPerc methodsFor: 'envelope coordinates'!
 envelope ^ Envelope levels: {0 . level . 0} times: {attackTime . releaseTime} curves: curves releaseNode: nil loopNode: nil offset: 0 !
!

Object subclass: #EnvLinen
 instanceVariableNames: 'attackTime sustainTime releaseTime level curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'!
!EnvLinen commentStamp: '<historical>' prior: 0! Trapezoidal envelope!

!EnvLinen methodsFor: 'accessing'!
 attackTime ^ attackTime !
 attackTime: x attackTime := x. !
 curves ^ curves !
 curves: x curves := x. !
 level ^ level !
 level: x level := x. !
 releaseTime ^ releaseTime !
 releaseTime: x releaseTime := x. !
 sustainTime ^ sustainTime !
 sustainTime: x sustainTime := x. !
!

!EnvLinen class methodsFor: 'instance creation'!
 attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curves: p5
 |u| u := self new . u attackTime: p1 . u sustainTime: p2 . u releaseTime: p3 . u level: p4 . u curves: p5 . ^ u !
!

!EnvLinen methodsFor: 'envelope coordinates'!
 envelope ^ Envelope levels: {0 . level . level . 0} times: {attackTime . sustainTime . releaseTime} curves: curves releaseNode: nil loopNode: nil offset: 0 !
!

Object subclass: #EnvASR
 instanceVariableNames: 'attackTime sustainLevel releaseTime curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!EnvASR commentStamp: '<historical>' prior: 0! Attack-Sustain-Release envelope!

!EnvASR methodsFor: 'accessing'!
 attackTime ^ attackTime !
 attackTime: x attackTime := x. !
 curves ^ curves !
 curves: x curves := x. !
 releaseTime ^ releaseTime !
 releaseTime: x releaseTime := x. !
 sustainLevel ^ sustainLevel !
 sustainLevel: x sustainLevel := x. !
!

!EnvASR class methodsFor: 'instance creation'!
 attackTime: p1 sustainLevel: p2 releaseTime: p3 curves: p4
 |u| u := self new . u attackTime: p1 . u sustainLevel: p2 . u releaseTime: p3 . u curves: p4 . ^ u !
!

!EnvASR methodsFor: 'envelope coordinates'!
 envelope ^ Envelope levels: {0 . sustainLevel . 0} times: {attackTime . releaseTime} curves: curves releaseNode: 1 loopNode: nil offset: 0 !
!

Object subclass: #EnvADSR
 instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!
!EnvADSR commentStamp: '<historical>' prior: 0! Attack-Decay-Sustain-Release envelope!

!EnvADSR methodsFor: 'accessing'!
 attackTime ^ attackTime !
 attackTime: x attackTime := x. !
 curves ^ curves !
 curves: x curves := x. !
 decayTime ^ decayTime !
 decayTime: x decayTime := x. !
 peakLevel ^ peakLevel !
 peakLevel: x peakLevel := x. !
 releaseTime ^ releaseTime !
 releaseTime: x releaseTime := x. !
 sustainLevel ^ sustainLevel !
 sustainLevel: x sustainLevel := x. !
!

!EnvADSR class methodsFor: 'instance creation'!
 attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curves: p6
 |u| u := self new . u attackTime: p1 . u decayTime: p2 . u sustainLevel: p3 . u releaseTime: p4 . u peakLevel: p5 . u curves: p6 . ^ u !
!

!EnvADSR methodsFor: 'envelope coordinates'!
 envelope ^ Envelope levels: {0 . peakLevel . peakLevel * sustainLevel . 0} times: {attackTime . decayTime . releaseTime} curves: curves releaseNode: 2 loopNode: nil offset: 0 !
!
