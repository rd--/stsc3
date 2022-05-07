!BlockClosure methodsFor: 'duplicating'!
dup: aNumber
    "Answer an array of aNumber places, each filled by sending #value to myself.

     [ 10 rand ] dup: 10
    "
    ^aNumber arrayFill: self!
dup
    "Answer an array of myself evaluated twice."
    ^self dup: 2!
!

!BlockClosure methodsFor: 'primary factory method'!
primaryFactoryMethod
    #valueWithArguments:!
!

!BlockClosure methodsFor: 'texture generation'!
overlapTexture: sustainTime value: transitionTime value: overlap
    ^OverlapTexture graphFunc: self sustainTime: sustainTime transitionTime: transitionTime overlap: overlap!
!

!BlockClosure methodsFor: 'scsynth'!
play
    self value play!
draw
    self value draw!
!
