AbstractUgen subclass: #Tr1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Tr1 comment: 'Trig1 with duration of one sample.'!

!Tr1 class methodsFor: 'instance creation'!
in: in
    ^Trig1 in: in dur: SampleDur new!
primaryFactoryMethod
    ^#in:!
!



