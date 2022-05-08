EnvBuilder subclass: #EnvAsr
  instanceVariableNames: 'attackTime sustainLevel releaseTime curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvAsr comment: 'Attack-Sustain-Release Envelope'!

!EnvAsr methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: sustainLevel with: 0)
      times: (Array with: attackTime with: releaseTime)
      curves: curves
      releaseNode: 1
      loopNode: nil
      offset: 0!
!

!EnvAsr methodsFor: 'initialisation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curves: p4
    attackTime := p1.
    sustainLevel := p2.
    releaseTime := p3.
    curves := p4!
!


!EnvAsr class methodsFor: 'instance creation'!
attackTime: p1 sustainLevel: p2 releaseTime: p3 curves: p4
    ^self new attackTime: p1 sustainLevel: p2 releaseTime: p3 curves: p4
!
primaryFactoryMethod
    ^#attackTime:sustainLevel:releaseTime:curves:!
!
