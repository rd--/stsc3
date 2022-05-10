!SmallInteger methodsFor: 'stsc3'!
+ aNumber
    <primitive: VMpr_SmallInteger_plus>
     aNumber isSmallInteger ifTrue: [ ^(LargeInteger fromInteger: self) + (LargeInteger fromInteger: aNumber) ].
    ^ aNumber adaptToInteger: self andSend: #+!
- aNumber
    <primitive: VMpr_SmallInteger_plus>
     aNumber isSmallInteger ifTrue: [ ^(LargeInteger fromInteger: self) - (LargeInteger fromInteger: aNumber) ].
    ^ aNumber adaptToInteger: self andSend: #-!
* aNumber
    <primitive: VMpr_SmallInteger_times>
     aNumber isSmallInteger ifTrue: [ ^(LargeInteger fromInteger: self) * (LargeInteger fromInteger: aNumber) ].
   ^ aNumber adaptToInteger: self andSend: #*!
/ aNumber
    <primitive: VMpr_SmallInteger_divide> "the primitive answers integer results, i.e. 4 / 2"
   ^ self asFloat / aNumber! "avoid fractions"
!
