EnvBuilder subclass: #EnvLinen
  instanceVariableNames: 'attackTime sustainTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvLinen comment: 'Trapezoidal Envelope (Fixed duration, no release node)'!

!EnvLinen methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: level with: 0)
      times: (Array with: attackTime with: sustainTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0!
!

!EnvLinen methodsFor: 'initialisation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
    attackTime := p1.
    sustainTime := p2.
    releaseTime := p3.
    level := p4.
    curve := p5!
!

!EnvLinen class methodsFor: 'instance creation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
    ^self new attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
!
primaryFactoryMethod
    ^#attackTime:sustainTime:releaseTime:level:curve:!
!
