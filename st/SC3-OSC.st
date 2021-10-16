!SmallInteger methodsFor: 'OpenSoundControl'!
  oscTypeTag ^$i !
  putOscOn: aStream
    aStream nextInt32Put: self
  !
  putFudiOn: aStream
    self asString putOn: aStream
  !
  oscAlign
    "OpenSoundControl Strings and ByteArrays are 32-bit aligned.
     Calculate bytes required to extend myself to a 4-byte boundary."
    ^((self + 3) bitAnd: 3 bitInvert) - self
  !
!

!Float methodsFor: 'OpenSoundControl'!
  oscTypeTag ^$f !
  putOscOn: aStream
    aStream nextFloat32Put: self
  !
  putFudiOn: aStream
    self asString putOn: aStream
  !
!

!String methodsFor: 'OpenSoundControl'!
  oscTypeTag ^$s !
  putOscOn: aStream
    "Strings are ASCII and null terminated and padded to be a multiple of 4-bytes in size"
    self isEmpty ifTrue: [ ^self error: 'empty string in osc message' ].
    aStream nextPutAll: self asByteArray.
    aStream nextPut: Character nul.
    (self size + 1) oscAlign timesRepeat: [ aStream nextPut: Character nul ]
  !
  putFudiOn: aStream
    self putOn: aStream
  !
!

!ByteArray methodsFor: 'OpenSoundControl'!
  oscTypeTag ^$b !
  putOscOn: aStream
    "ByteArray are length prefixed and padded to be a multiple of 4-bytes in size"
    self isEmpty ifTrue: [ ^self error: 'empty bytearray in osc message' ].
    self size putOscOn: aStream.
    aStream nextPutAll: self.
    self size oscAlign timesRepeat: [ aStream nextPut: Character nul ]
  !
  putFudiOn: aStream
    self error: 'Fudi does not have a standard bytestring notation, perhaps #-prefixed hexdata (c.f. xxd)'
  !
!

Object subclass: #OscMessage
  instanceVariableNames: 'address arguments'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Osc'
!

!OscMessage methodsFor: 'accessing'!
  address ^address!
  address: x address := x!
  arguments ^arguments!
  arguments: x arguments := x!
!

!OscMessage class methodsFor: 'instance creation'!
  at: address with: arguments
    "Make OpenSoundControl message at address with arguments"
    | m |
    m := self new.
    m address: address.
    m arguments: arguments.
    ^m
  !
  primaryFactoryMethod ^#at:with:!
!

!OscMessage methodsFor: 'OpenSoundControl'!
  descriptor
    ^',' , ((arguments collect: [ :each | each oscTypeTag ]) asString)
  !
  putOscOn: aStream
    address putOscOn: aStream.
    self descriptor putOscOn: aStream.
    arguments do: [ :each | each putOscOn: aStream ].
  !
  putFudiOn: aStream
    address putFudiOn: aStream.
    $ putOn: aStream.
    arguments do: [ :item | item putFudiOn: aStream ] separatedBy: [ $ putOn: aStream ].
    $; putOn: aStream
  !
!
