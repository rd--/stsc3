AbstractUgen subclass: #Rate
  instanceVariableNames: 'source rate'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Rate comment: 'Ugen Processing Rate'!
!Rate methodsFor: 'accessing'!
rate
    ^rate!
rate: aSymbol
    rate := aSymbol!
source
    ^source!
source: aUgen
    source := aUgen!
!

!Rate methodsFor: 'collecting'!
addToUgenSet: aSet
    source addToUgenSet: aSet!
!

!Rate methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    '(rewriteToRate ' putOn: fd.
    rate asString putOn: fd.
    fd space.
    source printHaskellOn: fd.
    ')' putOn: fd
  !
!


!Rate class methodsFor: 'instance creation'!
source: aUgen rate: aSymbol
    ^self new source: aUgen; rate: aSymbol; yourself!
!



