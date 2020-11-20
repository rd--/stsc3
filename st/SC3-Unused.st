"OutputProxy is only required if MCE is done at Smalltalk"

Object subclass: #OutputProxy
 instanceVariableNames: 'source outputIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !
!OutputProxy commentStamp: '<historical>' prior: 0! Output Proxy !

!OutputProxy methodsFor: 'accessing'!
 outputIndex ^ outputIndex !
 outputIndex: x outputIndex := x !
 source ^ source !
 source: x source := x !
!

!OutputProxy class methodsFor: 'instance creation'!
 source: source outputIndex: outputIndex ^self new source: source outputIndex: outputIndex ! !

"RotateStereo is a composite UGen at hsc3 - it cannot be used with the current haskell printer"

UGen subclass: #RotateStereo
 instanceVariableNames: 'in pos'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !
!OutputProxy commentStamp: '<historical>' prior: 0! Rotate2 with single stereo argument !

!RotateStereo methodsFor: 'private'!
 in: x in := x !
 pos: x pos := x ! !

!RotateStereo class methodsFor: 'instance creation'!
 in: p1 pos: p2 |u| u := self new . u in: p1 . u pos: p2 . ^ u ! !
