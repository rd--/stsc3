AbstractUgen subclass: #PrimitiveUgenOutput
  instanceVariableNames: 'ugen index'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PrimitiveUgenOutput comment: 'Output proxy'!

!PrimitiveUgenOutput methodsFor: 'initialization'!
ugen: aPrimitiveUgen index: anInteger
    ugen := aPrimitiveUgen.
    index := anInteger!
!

!PrimitiveUgenOutput class methodsFor: 'instance creation'!
ugen: aPrimitiveUgen index: anInteger
    ^self new ugen: aPrimitiveUgen index: anInteger!
!

!PrimitiveUgenOutput methodsFor: 'rate control'!
rate
    ^ugen rate!
kr
    ugen kr.
    ^self!
!

!PrimitiveUgenOutput methodsFor: 'mrg'!
mrg: rhs
    ugen mrg: rhs.
    ^self!
!

!PrimitiveUgenOutput methodsFor: 'testing'!
acceptsMrg
    ^true!
!

!PrimitiveUgenOutput methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    ugen addToUgenSet: c protectedBy: w!
!

!PrimitiveUgenOutput methodsFor: 'printing'!
printHaskellOn: fd
    "Print my Hsc3 form."
    '(mceChannel ' putOn: fd. (index - 1) asString putOn: fd. ' ' putOn: fd. ugen printHaskellOn: fd. ') ' putOn: fd!
!
