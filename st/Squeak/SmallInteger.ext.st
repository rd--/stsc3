!SmallInteger methodsFor: 'stsc3'!
printHexByte
    (self > 255 or: self < 0) ifTrue: [ self error: 'SmallInteger>>printHexByte' ].
    ^self printStringBase: 16 nDigits: 2!
!


