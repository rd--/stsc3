
AbstractUgen subclass: #Mix
  instanceVariableNames: 'source'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Mix comment: 'Ugen Mixer'!

!Mix methodsFor: 'collecting'!
  addToUgenSet: c
    source addToUgenSet: c
  !
!

!Mix methodsFor: 'accessing'!
  source ^source!
  source: x source := x!
!

!Mix class methodsFor: 'instance creation'!
  source: x
    ^self new source: x
  !
  primaryFactoryMethod ^#source:!
!
