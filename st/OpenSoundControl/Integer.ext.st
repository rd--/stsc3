!Integer methodsFor: 'OpenSoundControl'!
oscTypeTag
    ^$i!
putOscOn: aStream
    aStream int32: self!
printOscOn: aStream
    aStream << self asString << ' '!
oscAlign
    ^((self + 3) bitAnd: 3 bitInvert) - self!
!
