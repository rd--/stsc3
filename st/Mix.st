AbstractUgen subclass: #Mix
  instanceVariableNames: 'source'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Mix comment: 'Ugen Mixer'!
!Mix methodsFor: 'accessing'!
source
    ^source!
source: aSignal
    source := aSignal!
!

!Mix methodsFor: 'collecting'!
addToUgenSet: aSet
    source addToUgenSet: aSet!
!

!Mix methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    '(mix ' putOn: fd.
    source printHaskellOn: fd.
    ')' putOn: fd
  !
!


!Mix class methodsFor: 'instance creation'!
source: aSignal
    ^self new source: aSignal!
primaryFactoryMethod
    ^#source:!
!



