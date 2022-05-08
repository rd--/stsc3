Object subclass: #Voicer
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Voicer comment: 'Event Control Input'!

!Voicer class methodsFor: 'instance creation'!
voiceAddr: voiceNumber
      | eventAddr eventIncr eventZero |
      eventAddr := 13000.
      eventIncr := 10.
      eventZero := 0.
      ^eventAddr + (voiceNumber - 1 + eventZero * eventIncr)!
k0: k0 stp: stp c: c
    ^EventParam voice: c fromUgen: (ControlIn numChan: 9 bus: (k0 + (c * stp)))!
k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc
    | v |
    v := 0 to: numVoices - 1.
    ^v collect: [ :c | voiceFunc value: (self k0: k0 stp: stp c: (c0 + c))]!
numVoices: numVoices voiceFunc: voiceFunc
    | k0 stp c0 |
    k0 := 13000. "Control name: 'eventAddr' init: 13000."
    stp := 10. "Control name: 'eventIncr' init: 10."
    c0 := 0. "Control name: 'eventZero' init: 0."
    ^self k0: k0 stp: stp c0: c0 numVoices: numVoices voiceFunc: voiceFunc!
primaryFactoryMethod
    ^#numVoices:voiceFunc:!
!
