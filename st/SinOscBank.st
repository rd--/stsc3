PsuedoUgen subclass: #SinOscBank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

SinOscBank comment: 'Parallel bank of SinOsc'!

!SinOscBank class methodsFor: 'instance creation'!
freq: freq amp: amp phase: phase
    ^Klang freqscale: 1 freqoffset: 0 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: phase)!
primaryFactoryMethod
    ^#freq:amp:phase:!
!



