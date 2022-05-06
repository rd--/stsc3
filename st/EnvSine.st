EnvBuilder subclass: #EnvSine
  instanceVariableNames: 'dur level'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

EnvSine comment: 'Sine (Hanning Window) Envelope Builder (Fixed duration, no release node)'!

!EnvSine methodsFor: 'envelope specification'!
asEnvSpec
    ^EnvSpec
      levels: (Array with: 0 with: level with: 0)
      times: (Array with: dur / 2 with: dur / 2)
      curves: #(3 3)
      releaseNode: nil
      loopNode: 0
      offset: 0!
!

!EnvSine methodsFor: 'initialisation'!
dur: p1 level: p2
    dur := p1.
    level := p2!
!

!EnvSine class methodsFor: 'instance creation'!
dur: p1 level: p2
    ^self new dur: p1 level: p2!
primaryFactoryMethod
    ^#dur:level:!
!
