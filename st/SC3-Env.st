Object subclass: #Envelope
 instanceVariableNames: 'levels times curves releaseNode loopNode offset'
 classVariableNames: 'CurveNameDictionary'
 poolDictionaries: ''
 category: 'SC3' !

!Envelope methodsFor: 'accessing'!
curves ^ curves !
curves: anObject curves := anObject. !
levels ^ levels !
levels: anObject levels := anObject. !
loopNode ^ loopNode !
loopNode: anObject loopNode := anObject. !
offset ^ offset !
offset: anObject offset := anObject. !
releaseNode ^ releaseNode !
releaseNode: anObject releaseNode := anObject. !
times ^ times !
times: anObject times := anObject. ! !

!Envelope class methodsFor: 'accessing'!
curveNameDictionary ^ CurveNameDictionary ! !

!Envelope class methodsFor: 'instance creation'!
levels: p1 times: p2 curves: p3 releaseNode: p4 loopNode: p5 offset: p6
|u| u := self new . u levels: p1 . u times: p2 . u curves: p3 . u releaseNode: p4 . u loopNode: p5 . u offset: p6 .
^ u ! !

!Envelope class methodsFor: 'initialisation'!
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
 #hold -> 8 } ! !

!Envelope methodsFor: 'envelope coordinates'!
coord
|r n|
n := times size .
r := Array new: ((n + 1) * 4) .
r at: 1 put: (levels at: 1) .
r at: 2 put: n .
r at: 3 put: (releaseNode isNil ifTrue: -99 ifFalse: releaseNode) .
r at: 4 put: (loopNode isNil ifTrue: -99 ifFalse: loopNode) .
(1 to: n) do: [: i|
 r at: (1 + (i * 4)) put: (levels at: (i + 1)) .
 r at: (2 + (i * 4)) put: (times at: i) .
 r at: (3 + (i * 4)) put: (self class curveNameDictionary at: (curves at: i) ifAbsent: 5) .
 r at: (4 + (i * 4)) put: ((curves at: i) isSymbol ifTrue: 0 ifFalse: (curves at: i)) ] .
^r ! !

!Envelope methodsFor: 'envelope generator'!
gen: x ^ EnvGen gate: 1 levelScale: 1 levelBias: 0 timeScale: 1 doneAction: x envelope: self coord !
gen ^ self gen: 0 ! !

Object subclass: #EnvPerc
 instanceVariableNames: 'attackTime releaseTime level curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!EnvPerc methodsFor: 'accessing'!
attackTime ^ attackTime !
attackTime: anObject attackTime := anObject. !
curves ^ curves !
curves: anObject curves := anObject. !
level ^ level !
level: anObject level := anObject. !
releaseTime ^ releaseTime !
releaseTime: anObject releaseTime := anObject. ! !

!EnvPerc class methodsFor: 'instance creation'!
attackTime: p1 releaseTime: p2 level: p3 curves: p4
|u| u := self new . u attackTime: p1 . u releaseTime: p2 . u level: p3 . u curves: p4 . ^ u ! !

!EnvPerc methodsFor: 'envelope coordinates'!
envelope ^ Envelope levels: {0 . level . 0} times: {attackTime . releaseTime} curves: curves releaseNode: nil loopNode: nil offset: 0 ! !

Object subclass: #EnvLinen
 instanceVariableNames: 'attackTime sustainTime releaseTime level curves'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!EnvLinen methodsFor: 'accessing'!
attackTime ^ attackTime !
attackTime: anObject attackTime := anObject. !
curves ^ curves !
curves: anObject curves := anObject. !
level ^ level !
level: anObject level := anObject. !
releaseTime ^ releaseTime !
releaseTime: anObject releaseTime := anObject. !
sustainTime ^ sustainTime !
sustainTime: anObject sustainTime := anObject. ! !

!EnvLinen class methodsFor: 'instance creation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curves: p5
|u| u := self new . u attackTime: p1 . u sustainTime: p2 . u releaseTime: p3 . u level: p4 . u curves: p5 . ^ u ! !

!EnvLinen methodsFor: 'envelope coordinates'!
envelope ^ Envelope levels: {0 . level . level . 0} times: {attackTime . sustainTime . releaseTime} curves: curves releaseNode: nil loopNode: nil offset: 0 ! !
