PsuedoUgen subclass: #PMOsc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PMOsc comment: 'Phase modulation sine oscillator pair.'!

!PMOsc class methodsFor: 'instance creation'!
carfreq: cf modfreq: mf pmindex: pm modphase: mp
    ^SinOsc freq: cf phase: (SinOsc freq: mf phase: mp) * pm!
carfreq: cf modfreq: mf pmindex: pm modphase: mp mul: mul
    ^(self carfreq: cf modfreq: mf pmindex: pm modphase: mp) * mul!
primaryFactoryMethod
    ^#carfreq:modfreq:pmindex:modphase:!
!



