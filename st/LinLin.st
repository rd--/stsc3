PsuedoUgen subclass: #LinLin
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

LinLin comment: 'Map a linear range to a linear range'!

!LinLin class methodsFor: 'instance creation'!
in: in srclo: srclo srchi: srchi dstlo: dstlo dsthi: dsthi
    ^(in - srclo) / (srchi - srclo) * (dsthi - dstlo) + dstlo!
primaryFactoryMethod
    ^#in:srclo:srchi:dstlo:dsthi:!
!



