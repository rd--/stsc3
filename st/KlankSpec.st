Object subclass: #KlankSpec
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

KlankSpec comment: 'Format data for Klank Ugen'!

!KlankSpec class methodsFor: 'instance creation'!
freq: freq amp: amp time: time
    "Reshape input arrays, and allow amp and time to be nil (defaulting to 1)"
    ^{freq. amp ifNil: [freq size replicate: 1]. time ifNil: [freq size replicate: 1]} extendToBeOfEqualSize transpose concatenation!
primaryFactoryMethod
    ^#freq:amp:time:!
!
