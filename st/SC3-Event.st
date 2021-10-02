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

AbstractUGen subclass: #EventControl
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

EventControl comment: 'Event Control Input'!

!EventControl class methodsFor: 'instance creation'!
  k0: k0 stp: stp c: c
    ^EventParam voice: c fromUGen: (In numChan: 9 bus: (k0 + (c * stp))) kr asMce.
  !
  k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc
    | v |
    v := 0 to: numVoices - 1.
    ^(v collect: [ :c | voiceFunc value: (self k0: k0 stp: stp c: (c0 + c))]) asUGen
  !
  numVoices: numVoices voiceFunc: voiceFunc
    | k0 stp c0 |
    k0 := Control name: 'eventAddr' init: 13000.
    stp := Control name: 'eventIncr' init: 10.
    c0 := Control name: 'eventZero' init: 0.
    ^self k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc.
  !
  primaryFactoryMethod ^#numVoices:voiceFunc:!
!
