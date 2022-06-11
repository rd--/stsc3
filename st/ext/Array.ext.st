!Array methodsFor: 'mce'!
asArray
    ^self!
!

!Array methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self!
!

!Array methodsFor: 'klang data'!
asKlangSpec
    | default |
    self size = 3 ifFalse: [ self error: 'asKlangSpec: invalid input array' ].
    default := [ 1 dup: (self at: 1) size ].
    ^{self at: 1. (self at: 2) ifNil: default. (self at: 3) ifNil: default} extendToBeOfEqualSize transpose concatenation!
asKlankSpec
    ^self asKlangSpec!
!

!Array methodsFor: 'unit generator graph'!
play
    Sc3 play: self!
draw
    Sc3 draw: self!
printUgens
    Sc3 printUgens: self!
!
