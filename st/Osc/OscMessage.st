Object subclass: #OscMessage
  instanceVariableNames: 'address arguments'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Osc'!

!OscMessage methodsFor: 'accessing'!
address
    ^address!
address: aString
     address := aString!
arguments
    ^arguments!
arguments: anArray
    arguments := anArray!
!

!OscMessage class methodsFor: 'instance creation'!
at: address with: arguments
    ^self new address: address; arguments: arguments; yourself!
primaryFactoryMethod
    ^#at:with:!
!

!OscMessage methodsFor: 'OpenSoundControl'!
descriptor
    ^',' , ((arguments collect: [ :item | item oscTypeTag ]) asString)!
putOscOn: aStream
    address putOscOn: aStream.
    self descriptor putOscOn: aStream.
    arguments do: [ :item | item putOscOn: aStream ]!
printOscOn: aStream
    address printOscOn: aStream.
    self descriptor printOscOn: aStream.
    arguments do: [ :item | item printOscOn: aStream ]!
encodeOsc
    ^ByteArray streamContents: [ :aStream | self putOscOn: aStream ]!
sendToHost: hostAddress atPort: portNumber
    | socket |
    socket := Socket newUDP.
    socket sendUDPData: self encodeOsc toHost: hostAddress port: portNumber.
    socket close!
!
