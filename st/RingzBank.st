PsuedoUgen subclass: #RingzBank
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

RingzBank comment: 'Parallel bank of Ringz'!

!RingzBank class methodsFor: 'instance creation'!
input: input freq: freq amp: amp time: time
    ^Klank input: input freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: (KlankSpec freq: freq amp: amp time: time)!
primaryFactoryMethod
    ^#input:freq:amp:time:!
!



