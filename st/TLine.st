PsuedoUgen subclass: #TLine
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

TLine comment: 'Triggered Line.'!

!TLine class methodsFor: 'instance creation'!
start: start end: end dur: dur trig: trig
    ^(EnvSpec
               levels: (Array with: start with: start with: end)
               times: (Array with: 0 with: dur)
               curves: (Array with: #lin with: #lin)
               releaseNode: -1 loopNode: -1 offset: 0) asEnvGen: trig!
primaryFactoryMethod
    ^#start:end:dur:trig:!
!



