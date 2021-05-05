Object subclass: #EventParam
 instanceVariableNames: 'w x y z o rx ry p px'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

EventParam comment: 'Event Input Parameters'!

!EventParam methodsFor: 'accessing'!
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
 fromUGen: u
  "Create Event parameter object from MCE UGen"
  | e |
  e := self new.
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

Object subclass: #EventAddr
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

!EventAddr class methodsFor: 'instance creation'!
 k0: k0 c: c
  ^EventParam fromUGen: (In numChan: 9 bus: (k0 + (c * 10))) kr mce.
 !
!

Object subclass: #EventVoicerAddr
 instanceVariableNames: ''
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

!EventVoicerAddr class methodsFor: 'instance creation'!
 k0: k0 c0: c0 numVoices: numVoices voiceFunc: voiceFunc
  | v |
  v := (Interval from: 0 to: (numVoices - 1)) asArray.
  ^(v collect: [:c | voiceFunc value: (EventAddr k0: k0 c: (c0 + c)) value: c]) mce
 !
!
