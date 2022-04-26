AbstractUgen subclass: #Mce
  instanceVariableNames: 'source transform'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mce comment: 'Multiple Channel Expansion'!


!Mce methodsFor: 'collecting'!
  addToUgenSet: c
    source addToUgenSet: c
  !
!

!Mce methodsFor: 'accessing'!
  source: x source := x!
  source ^source!
  transform: x transform := x!
  transform ^transform!
  size ^source size!
!

!Mce methodsFor: 'initialization'!
  initialize
    source := nil.
    transform := nil
  !
!

!Mce class methodsFor: 'instance creation'!
  source: x
    ^self new source: x
  !
  with: c1 with: c2
    ^Mce source: (Array with: c1 with: c2)
  !
!

!Mce methodsFor: 'transforms'!
  reverse
    ^self copy transform: #(#mceReverse)
  !
  transpose
    ^self copy transform: #(#mceTranspose)
  !
  at: x
    ^self copy transform: (Array with: #mceChannel with: x - 1)
  !
  atWrap: x
    ^self copy transform: (Array with: #mceChannelWrap with: x - 1)
  !
  clump: x
    ^self copy transform: (Array with: #mceClump with: x)
  !
  product
    ^self copy transform: #(#mceProduct)
  !
  rotate: n
    ^self copy transform: (Array with: #mceRotate with: n)
  !
!
