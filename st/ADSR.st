PsuedoUgen subclass: #ADSR
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ADSR comment: 'EnvGen of simplified EnvADSR'!

!ADSR class methodsFor: 'instance creation'!
gate: g attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 curve: p5
    ^(EnvADSR attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: 1 curve: p5) asEnvGen: g
!
primaryFactoryMethod
    ^#gate:attackTime:decayTime:sustainLevel:releaseTime:curve:!
!



