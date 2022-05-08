EnvBuilder subclass: #EnvAdsr
  instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curves'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvAdsr comment: 'Attack-Decay-Sustain-Release Envelope'!

!EnvAdsr methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: peakLevel with: peakLevel * sustainLevel with: 0)
      times: (Array with: attackTime with: decayTime with: releaseTime)
      curves: curves
      releaseNode: 2
      loopNode: nil
      offset: 0!
!

!EnvAdsr methodsFor: 'initialisation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curves: p6
    attackTime := p1.
    decayTime := p2.
    sustainLevel := p3.
    releaseTime := p4.
    peakLevel := p5.
    curves := p6!
!

!EnvAdsr class methodsFor: 'instance creation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curves: p6
    ^self new attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curves: p6
!
primaryFactoryMethod
    ^#attackTime:decayTime:sustainLevel:releaseTime:peakLevel:curves:!
!
