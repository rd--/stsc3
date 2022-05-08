!Object methodsFor: 'array'!
asArray
    ^Array with: self!
dup: anInteger sending: aSelector
    "Answer an array of anInteger places, each filled by sending aSelector to myself."
    | answer |
    answer := Array new: anInteger.
    (1 to: anInteger) do: [ :index | answer at: index put: (self perform: aSelector) ].
    ^answer!
dup: anInteger
    ^self dup: anInteger sending: #copy!
dup
    ^self dup: 2!
!

!Object methodsFor: 'ugen'!
isUgen
    ^false!
!

!Object class methodsFor: 'primary factory method'!
apply: argumentsArray
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray!
!
