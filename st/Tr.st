PsuedoUgen subclass: #Tr
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Tr comment: 'Trig with duration of one sample.'!

!Tr class methodsFor: 'instance creation'!
in: in
    ^Trig in: in dur: SampleDur new "Should be SampleDur if audio rate and ControlDur if control rate, requires QuantaDur or some such."!
primaryFactoryMethod
    ^#in:!
!



