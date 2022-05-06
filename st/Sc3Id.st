Object subclass: #Sc3Id
  instanceVariableNames: ''
  classVariableNames: 'IdCounter'
  poolDictionaries: ''
  category: 'Sound-Sc3'!

!Sc3Id class methodsFor: 'identifiers'!
nextId
    "Generate a new identifier.  Avoid requiring class initialisation by checking for nil.
    (1 to: 9) collect: [ :unusedItem | Sc3Id nextId ]
    "
    IdCounter isNil ifTrue: [ IdCounter := 0 ] ifFalse: [ IdCounter := IdCounter + 1 ].
    ^IdCounter!
!
