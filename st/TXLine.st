AbstractUgen subclass: #TXLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TXLine comment: 'Triggered XLine.'!

!TXLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: #(#exp #exp)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:!
!



