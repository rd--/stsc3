AbstractUgen subclass: #Mix
  instanceVariableNames: 'source'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Mix comment: 'Ugen Mixer'!

!Mix methodsFor: 'collecting'!
addToUgenSet: aSet
    source addToUgenSet: aSet! !

!Mix methodsFor: 'accessing'!
source
    ^source!
source: aSignal
    source := aSignal! !

!Mix class methodsFor: 'instance creation'!
source: aSignal
    ^self new source: aSignal!
primaryFactoryMethod
    ^#source:! !
