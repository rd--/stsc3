PsuedoUgen subclass: #ASR
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ASR comment: 'EnvGen of simplified EnvASR'!

!ASR class methodsFor: 'instance creation'!
gate: g attackTime: p1 releaseTime: p2 curve: p3
    ^(EnvASR attackTime: p1 sustainLevel: 1 releaseTime: p2 curve: p3) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:releaseTime:curve:!
!



