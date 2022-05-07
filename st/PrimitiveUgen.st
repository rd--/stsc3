AbstractUgen subclass: #PrimitiveUgen
  instanceVariableNames: 'name numChan rate specialIndex inputArray id mrg'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PrimitiveUgen comment: 'Primitive unit generator.
Objects have identity, the id field is used to sort unit generators into applicative order.'!

!PrimitiveUgen methodsFor: 'initialization'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    name := nm.
    numChan := nc.
    rate := rt.
    specialIndex := ix.
    inputArray := in.
    id := Sc3Id nextId.
    mrg := OrderedCollection new!
!

!PrimitiveUgen class methodsFor: 'instance creation'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    ^self new name: nm numChan: nc rate: rt specialIndex: ix inputArray: in!
!

!PrimitiveUgen methodsFor: 'accessing'!
id
    ^id!
id: aNumber
    id := aNumber!
rate
    ^rate!
rate: aNumber
    rate := aNumber!
!

!PrimitiveUgen methodsFor: 'rate control'!
kr
    (rate = Rate ar) ifTrue: [rate := Rate kr].
    inputArray kr.
    ^self!
!

!PrimitiveUgen methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    (w includes: self) ifFalse: [
        c add: self.
        inputArray do: [ :item | item addToUgenSet: c protectedBy: w ].
        mrg do: [ :item | item addToUgenSet: c protectedBy: c ] ]!
!

!PrimitiveUgen methodsFor: 'mrg'!
mrg: rhs
    rhs asArray do: [ :item | mrg add: item ].
    ^self.
!

!PrimitiveUgen methodsFor: 'testing'!
acceptsMrg
    ^true!
!

"Mrg needs to be printed here.
It can't be at printHaskellOn because that will be used on the lhs of =, and at printHaskellDefinitionOn: it makes a loop.
It requires an extra id, so another node, so Mrg...
Should just print a text syndef."
!PrimitiveUgen methodsFor: 'printing'!
printHaskellOn: fd
    "Print the name I am bound to in hsc3."
    '_u_', id asString putOn: fd!
printHaskellDefinitionOn: fd
    "Print my plain Hsc3 definition."
    | mce nc |
    '(mk_plain ' putOn: fd.
    (Rate symbol: rate) asString putOn: fd. fd space.
    name asString printHaskellOn: fd. fd space.
    '[' putOn: fd. inputArray do: [ :item | item printHaskellOn: fd. fd space ] separatedBy: [ ',' putOn: fd ]. '] ' putOn: fd.
    numChan asString putOn: fd. fd space.
    '(Special ' putOn: fd.  specialIndex asString putOn: fd. ') ' putOn: fd.
    '(UId ' putOn: fd. id asString putOn: fd. ')' putOn: fd.
    ') ' putOn: fd!
!
