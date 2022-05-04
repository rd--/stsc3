EnvBuilder subclass: #EnvASR
  instanceVariableNames: 'attackTime sustainLevel releaseTime curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvASR comment: 'Attack-Sustain-Release Envelope'!
!EnvASR methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: sustainLevel with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curve
      releaseNode: 1
      loopNode: nil
      offset: 0!
!

!EnvASR methodsFor: 'initialisation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
    attackTime := p1.
    sustainLevel := p2.
    releaseTime := p3.
    curve := p4!
!


!EnvASR class methodsFor: 'instance creation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
    ^self new attackTime: p1 sustainLevel: p2 releaseTime: p3 curve: p4
!
primaryFactoryMethod
    ^#attackTime:sustainLevel:releaseTime:curve:!
!



