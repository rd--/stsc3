!Array methodsFor: 'OpenSoundControl'!
oscTypeTag
    ^$b!
putOscOn: aStream
    self isEmpty ifTrue: [ ^self error: 'Array>>putOscOn: isEmpty' ].
    self size putOscOn: aStream.
    aStream nextPutAll: self asByteArray.
    self size oscAlign timesRepeat: [ aStream nextPut: Character nul ]!
printOscOn: aStream
    self isEmpty ifTrue: [ ^self error: 'Array>>printOscOn: isEmpty' ].
    self do: [ :item | aStream << item printHexByte ]!
!
