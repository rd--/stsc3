Object subclass: #REventParam
 instanceVariableNames: 'w x y z o rx ry p px'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

REventParam comment: 'Event Input Parameters'!

!REventParam methodsFor: 'accessing'!
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

!REventParam class methodsFor: 'instance creation'!
 fromUGen: p
  "Create Event parameter object from MCE UGen"
  | e |
  e := self new.
  e w: (p at: 1).
  e x: (p at: 2).
  e y: (p at: 3).
  e z: (p at: 4).
  e o: (p at: 5).
  e rx: (p at: 6).
  e ry: (p at: 7).
  e p: (p at: 8).
  e px: (p at: 9).
  ^e
 !
!
