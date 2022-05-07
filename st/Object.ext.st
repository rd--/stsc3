!Object methodsFor: 'array'!
asArray
    ^Array with: self!
!

!Object methodsFor: 'ugen'!
isUgen
    ^false!
!

!Object class methodsFor: 'primary factory method'!
apply: argumentsArray
    ^self perform: (self primaryFactoryMethod) withArguments: argumentsArray!
!
