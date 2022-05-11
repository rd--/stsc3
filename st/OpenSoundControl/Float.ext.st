!Float methodsFor: 'OpenSoundControl'!
oscTypeTag
    ^$f!
putOscOn: aStream
    aStream float32: self!
printOscOn: aStream
    aStream << self asString << ' '!
!
