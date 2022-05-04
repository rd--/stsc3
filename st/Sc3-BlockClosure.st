!BlockClosure methodsFor: 'enumerating'!
dup: aNumber
    "An array of aNumber places, each filled by sending value to myself.
     Sc allows an index argument, however Sc and St disagree about indexing.
    [ 10 rand ] dup: 10
    "
    ^aNumber arrayFill: self!
dup
    "An array of myself evaluated twice."
    ^self dup: 2! !

!BlockClosure methodsFor: 'application'!
apply: anArray
    "Alias for valueWithArguments:.
    For .stc notation.
    Allow f(x,y) as notation to apply a block f to arguments x & y."
    ^self valueWithArguments: anArray! !

!BlockClosure methodsFor: 'texture generation'!
overlapTexture: sustainTime value: transitionTime value: overlap
    ^OverlapTexture graphFunc: self sustainTime: sustainTime transitionTime: transitionTime overlap: overlap! !

!BlockClosure methodsFor: 'scsynth'!
play
    self value play!
draw
    self value draw! !
