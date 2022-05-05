PseudoUgen subclass: #BufAlloc
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

BufAlloc comment: 'Allocate a buffer local to the synth.'!

!BufAlloc class methodsFor: 'instance creation'!
numChannels: numChannels numFrames: numFrames
    ^LocalBuf numChannels: numChannels numFrames: numFrames!
primaryFactoryMethod
    ^#numChannels:numFrames:!
!



