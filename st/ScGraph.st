Object subclass: #ScGraph
  instanceVariableNames: 'name ugenSeq constantSeq'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ScGraph comment: 'SuperCollider unit generator graph.'!

!ScGraph methodsFor: 'initialization'!
name: nm ugenSeq: us constantSeq: cs
    name := nm. ugenSeq := us. constantSeq := cs!
!

!ScGraph class methodsFor: 'instance creation'!
from: anObject withName: aString
    | leafNodes ugens constants numLocalBufs maxLocalBufs ugenSeq constantSeq |
    leafNodes := (Sc3 ugenSetOf: anObject) asArray.
    ugens := (leafNodes reject: [ :item | item isNumber ]) sorted: [ :lhs :rhs | lhs id <= rhs id ].
    constants := leafNodes select: [ :item | item isNumber ].
    numLocalBufs := (ugens select: [ :item | item name = 'LocalBuf' ]) size.
    maxLocalBufs := UgenBuilder name: 'MaxLocalBufs' numChan: 1 rateSpec: Rate ir specialIndex: 0 inputArray: {numLocalBufs}.
    ugenSeq := {maxLocalBufs scUgen} , ugens.
    constantSeq := ({numLocalBufs} , constants) asSet asArray sorted collect: [ :item | item asFloat ].
    ^ScGraph new name: aString ugenSeq: ugenSeq constantSeq: constantSeq!
!

!ScGraph methodsFor: ''!
constantIndex: aNumber
    | answer |
    answer := constantSeq indexOf: (aNumber asFloat) ifAbsent: [ self error: ('cannot find index for number: ' , aNumber asString) ].
    ^answer - 1!
ugenIndex: aNumber
    ^(ugenSeq findIndex: [ :item | item id = aNumber ]) - 1!
ugenInputSpec: aValue
    aValue isNumber ifTrue: [
        ^{-1. self constantIndex: aValue}
    ] ifFalse: [
        ^{self ugenIndex: (aValue scUgen id). aValue port - 1}
    ]!
scgf
    ^1396926310!
printSyndefOn: aStream
    aStream
        << self scgf << ' '
        << 2 << ' ' "file version"
        << 1 << ' ' "# synth definitions"
        << name << ' '
        << constantSeq size << ' '.
    constantSeq do: [ :item | aStream << item asFloat << ' ' ].
    aStream
        << 0 << ' ' "# param"
        << 0 << ' ' "# param names"
        << ugenSeq size << ' '.
    ugenSeq do: [ :item | item printForGraph: self on: aStream ].
    aStream << 0 "# variants"!
!