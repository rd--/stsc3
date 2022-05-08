Object subclass: #KlankSpec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KlankSpec comment: 'Format data for Klank Ugen'!

!KlankSpec class methodsFor: 'instance creation'!
freq: freq amp: amp time: time
    "Reshape input arrays, and allow amp and time to be nil (defaulting to 1)"
    ^{freq. amp ifNil: [ 1 dup: freq size ]. time ifNil: [ 1 dup: freq size ]} extendToBeOfEqualSize transpose concatenation!
primaryFactoryMethod
    ^#freq:amp:time:!
!
