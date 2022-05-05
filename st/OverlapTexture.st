AbstractUgen subclass: #OverlapTexture
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

OverlapTexture comment: 'Texture Generator'!

!OverlapTexture class methodsFor: 'texture generation'!
graphFunc: graphFunc sustainTime: sustainTime transitionTime: transitionTime overlap: overlap
    ^(1 to: overlap) mixFill: [ :i |
      | t s e |
      t := Impulse freq: (1 / (sustainTime + (transitionTime * 2))) phase: (i / overlap).
      s := graphFunc value: t kr.
      e := EnvLinen attackTime: transitionTime sustainTime: sustainTime releaseTime: transitionTime level: 1 curve: #sin.
      s * (e asEnvGen: t withDoneAction: 0) ]!
primaryFactoryMethod
    ^#graphFunc:sustainTime:transitionTime:overlap:!
!


