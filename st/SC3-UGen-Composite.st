!LocalBuf class methodsFor: 'instance creation'!
 newFrom: x
 |p q |
 p := LocalBuf numChannels: 1 numFrames: x size .
 q := SetBuf buf: p offset: 0 length: x size array: x .
 ^ MRG lhs: p rhs: q ! !

UGen subclass: #Splay
 instanceVariableNames: 'input spread level center levelComp'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !
!Splay commentStamp: '<historical>' prior: 0!
Pan a set of channels across the stereo field. !

!Splay methodsFor: 'accessing'!
spread ^spread !
level ^level !
center ^center !
levelComp ^levelComp !
input ^input !
!

!Splay methodsFor: 'private'!
spread: x spread := x !
level: x level := x !
center: x center := x !
levelComp: x levelComp := x !
input: x input := x !
!

!Splay class methodsFor: 'instance creation'!
spread: spread level: level center: center levelComp: levelComp input: input |u| u := self new . u spread: spread . u level: level . u center: center . u levelComp: levelComp . u input: input . ^ u ! !

"RotateStereo is a composite UGen at hsc3 - it cannot be used with the current haskell printer"

UGen subclass: #RotateStereo
 instanceVariableNames: 'in pos'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!RotateStereo methodsFor: 'private'!
 in: x in := x !
 pos: x pos := x ! !

!RotateStereo class methodsFor: 'instance creation'!
 in: p1 pos: p2 |u| u := self new . u in: p1 . u pos: p2 . ^ u ! !
