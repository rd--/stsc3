!String methodsFor: 'OpenSoundControl'!
oscTypeTag
    ^$s!
putOscOn: aStream
    self isEmpty ifTrue: [ ^self error: 'String>>putOscOn: isEmpty' ].
    aStream nextPutAll: self asByteArray.
    aStream nextPut: Character nul.
    (self size + 1) oscAlign timesRepeat: [ aStream nextPut: Character nul ]!
printOscOn: aStream
    aStream << self << ' '!
!
