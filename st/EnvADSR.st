EnvBuilder subclass: #EnvADSR
  instanceVariableNames: 'attackTime decayTime sustainLevel releaseTime peakLevel curve'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvADSR comment: 'Attack-Decay-Sustain-Release Envelope'!
!EnvADSR methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: peakLevel with: peakLevel * sustainLevel with: 0)
      times: (Array with: attackTime with: decayTime with: releaseTime)
      curves: curve
      releaseNode: 2
      loopNode: nil
      offset: 0!
!

!EnvADSR methodsFor: 'initialisation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
    attackTime := p1.
    decayTime := p2.
    sustainLevel := p3.
    releaseTime := p4.
    peakLevel := p5.
    curve := p6!
!


!EnvADSR class methodsFor: 'instance creation'!
attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
    ^self new attackTime: p1 decayTime: p2 sustainLevel: p3 releaseTime: p4 peakLevel: p5 curve: p6
!
primaryFactoryMethod
    ^#attackTime:decayTime:sustainLevel:releaseTime:peakLevel:curve:!
!



