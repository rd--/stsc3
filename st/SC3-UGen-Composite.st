!LocalBuf class methodsFor: 'instance creation'!
 newFrom: x
  |p q |
  p := self numChannels: 1 numFrames: x size .
  q := SetBuf buf: p offset: 0 length: x size array: x .
  ^ MRG lhs: p rhs: q !
!

UGen subclass: #Splay
 instanceVariableNames: 'input spread level center levelComp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

Splay comment: 'Pan a set of channels across the stereo field.'!

!Splay methodsFor: 'accessing'!
 center ^center !
 center: x center := x !
 input ^input !
 input: x input := x !
 level ^level !
 level: x level := x !
 levelComp ^levelComp !
 levelComp: x levelComp := x !
 spread ^spread !
 spread: x spread := x !
!

!Splay class methodsFor: 'instance creation'!
 spread: spread level: level center: center levelComp: levelComp input: input
  |u| u := self new .
  u spread: spread .
  u level: level .
  u center: center .
  u levelComp: levelComp .
  u input: input .
  ^ u !
!
