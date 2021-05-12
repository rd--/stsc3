!LocalBuf class methodsFor: 'instance creation'!
 newFrom: x
  | p q |
  p := self numChannels: 1 numFrames: x size.
  q := SetBuf buf: p offset: 0 length: x size array: x.
  ^MRG lhs: p rhs: q
 !
!

AbstractUGen subclass: #Splay
 instanceVariableNames: 'input spread level center levelComp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3'
!

Splay comment: 'Pan a set of channels across the stereo field.'!

!Splay methodsFor: 'collecting'!
 addToUGenSet: c
  input addToUGenSet: c.
  spread addToUGenSet: c.
  level addToUGenSet: c.
  center addToUGenSet: c
 !
!

!Splay methodsFor: 'accessing'!
 center ^center!
 center: x center := x!
 input ^input!
 input: x input := x!
 level ^level!
 level: x level := x!
 levelComp ^levelComp!
 levelComp: x levelComp := x!
 spread ^spread!
 spread: x spread := x!
!

!Splay class methodsFor: 'instance creation'!
 input: input spread: spread level: level center: center levelComp: levelComp
  | u |
  u := self new.
  u input: input.
  u spread: spread.
  u level: level.
  u center: center.
  u levelComp: levelComp.
  ^u
 !
!

!AbstractUGen methodsFor: 'arithmetic'!
 inExpRangeFrom: x to: y
  ^LinExp in: self srclo: (-1) srchi: 1 dstlo: x dsthi: y
 !
 inRangeFrom: x to: y
  | m a |
  m := (y - x) / 2.
  a := x + m.
  ^self mul: m add: a.
 !
!
