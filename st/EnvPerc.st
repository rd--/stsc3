EnvBuilder subclass: #EnvPerc
  instanceVariableNames: 'attackTime releaseTime level curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvPerc comment: 'Attack-Release Envelope (Fixed duration, no release node)'!

!EnvPerc methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: nil
      loopNode: nil
      offset: 0!
!

!EnvPerc methodsFor: 'initialisation'!
attackTime: p1 releaseTime: p2 level: p3 curve: p4
    attackTime := p1.
    releaseTime := p2.
    level := p3.
    curve := p4!
!

!EnvPerc class methodsFor: 'instance creation'!
attackTime: p1 releaseTime: p2 level: p3 curve: p4
    ^ self new attackTime: p1 releaseTime: p2 level: p3 curve: p4
!
primaryFactoryMethod
     ^#attackTime:releaseTime:level:curve:!
!
