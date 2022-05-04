!LocalBuf class methodsFor: 'instance creation'!
newFrom: anArray
    | lhs rhs |
    lhs := self numChannels: 1 numFrames: anArray size.
    rhs := SetBuf buf: lhs offset: 0 length: anArray size array: anArray.
    ^Mrg lhs: lhs rhs: rhs!
!

