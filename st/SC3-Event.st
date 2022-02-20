Object subclass: #EventParam
  instanceVariableNames: 'v w x y z o rx ry p px'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EventParam comment: 'Event Parameters'!

!EventParam methodsFor: 'accessing'!
  v ^v!
  v: x v := x!
  w ^w!
  w: x w := x!
  x ^x!
  x: i x := i!
  y ^y!
  y: x y := x!
  z ^z!
  z: x z := x!
  o ^o!
  o: x o := x!
  rx ^rx!
  rx: x rx := x!
  ry ^ry!
  ry: x ry := x!
  p ^p!
  p: x p := x!
  px ^px!
  px: x px := x!
!

!EventParam class methodsFor: 'instance creation'!
  voice: v fromUGen: u
    "Create EventParam object from Mce UGen"
    | e |
    e := self new.
    e v: v.
    e w: (u at: 1).
    e x: (u at: 2).
    e y: (u at: 3).
    e z: (u at: 4).
    e o: (u at: 5).
    e rx: (u at: 6).
    e ry: (u at: 7).
    e p: (u at: 8).
    e px: (u at: 9).
    ^e
  !
!

AbstractUGen subclass: #Voicer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

Voicer comment: 'Event Control Input'!

!Voicer class methodsFor: 'instance creation'!
  voiceAddr: voiceNumber
      | eventAddr eventIncr eventZero |
      eventAddr := 13000.
      eventIncr := 10.
      eventZero := 0.
      ^ eventAddr + (voiceNumber - 1 + eventZero * eventIncr)
  !
  k0: k0 stp: stp c: c
    ^EventParam voice: c fromUGen: (ControlIn numChan: 9 bus: (k0 + (c * stp))) asMce.
  !
  k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc
    | v |
    v := 0 to: numVoices - 1.
    ^(v collect: [ :c | voiceFunc value: (self k0: k0 stp: stp c: (c0 + c))]) asUGen
  !
  numVoices: numVoices voiceFunc: voiceFunc
    | k0 stp c0 |
    k0 := 13000. "Control name: 'eventAddr' init: 13000."
    stp := 10. "Control name: 'eventIncr' init: 10."
    c0 := 0. "Control name: 'eventZero' init: 0."
    ^self k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc.
  !
  primaryFactoryMethod ^#numVoices:voiceFunc:!
!

AbstractUGen subclass: #KeyDown instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!KeyDown class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #KeyTimbre instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!KeyTimbre class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #KeyVelocity instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!KeyVelocity class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #KeyPitch instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!KeyPitch class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 7 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenDown instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenDown class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 0 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenX instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenX class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 1 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenY instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenY class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 2 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenZ instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenZ class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 3 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenAngle instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenAngle class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 4 !
  primaryFactoryMethod ^#voiceNumber:!
!

AbstractUGen subclass: #PenRadius instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-SC3'!
!PenRadius class methodsFor: 'instance creation'!
  voiceNumber: voiceNumber ^ControlIn numChan: 1 bus: (Voicer voiceAddr: voiceNumber) + 5 !
  primaryFactoryMethod ^#voiceNumber:!
!
