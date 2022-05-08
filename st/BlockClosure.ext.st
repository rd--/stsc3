!BlockClosure methodsFor: 'duplicating'!
dup: anInteger
    ^self dup: anInteger sending: #value!
!

!BlockClosure methodsFor: 'primary factory method'!
apply: anArray
    ^self valueWithArguments: anArray!
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
