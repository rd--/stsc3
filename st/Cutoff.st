AbstractUgen subclass: #Cutoff
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Cutoff comment: 'EnvGen of simple cutoff envelope'!

!Cutoff class methodsFor: 'instance creation'!
sustainTime: p1 releaseTime: p2 curve: p3
    ^(EnvSpec levels: #(1 1 0) times: {p1. p2} curves: p3 releaseNode: nil loopNode: nil offset: 0)  asEnvGen
!
primaryFactoryMethod
    ^#sustainTime:releaseTime:curve:!
!



