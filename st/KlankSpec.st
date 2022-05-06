AbstractUgen subclass: #KlankSpec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KlankSpec comment: 'Format data for Klank Ugen'!

!KlankSpec class methodsFor: 'instance creation'!
freq: freq amp: amp time: time
    "Reshape input arrays, and allow amp and time to be nil (defaulting to 1)"
    | n |
    n := freq size.
    ^{freq. amp ifNil: [n replicate: 1]. time ifNil: [n replicate: 1]} extendToBeOfEqualSize transpose concatenation!
primaryFactoryMethod
    ^#freq:amp:time:!
!
