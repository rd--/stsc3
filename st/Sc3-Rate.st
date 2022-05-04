AbstractUgen subclass: #Rate
  instanceVariableNames: 'source rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Rate comment: 'Ugen Processing Rate'!

!Rate methodsFor: 'collecting'!
addToUgenSet: aSet
    source addToUgenSet: aSet
!!

!Rate methodsFor: 'accessing'!
rate
    ^rate
!
rate: aSymbol
    rate := aSymbol
!
source
    ^source
!
source: aUgen
    source := aUgen
!!

!Rate class methodsFor: 'instance creation'!
source: aUgen rate: aSymbol
    ^self new source: aUgen; rate: aSymbol; yourself
!!
