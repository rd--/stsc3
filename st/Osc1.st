PsuedoUgen subclass: #Osc1
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Osc1 comment: 'An oscillator that reads through a table once.'!

!Osc1 class methodsFor: 'instance creation'!
bufnum: bufnum dur: dur
    | phase |
    phase := Ln start: 0 end: (BufFrames bufnum: bufnum) - 1 dur: dur.
    ^BufRd numChan: 1 bufnum: bufnum phase: phase loop: 0 interpolation: 2!
primaryFactoryMethod
    ^#bufnum:dur:!
!
