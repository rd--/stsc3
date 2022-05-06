AbstractUgen subclass: #PrimitiveUgen
  instanceVariableNames: 'name numChan rate specialIndex inputArray id'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PrimitiveUgen comment: 'Primitive unit generator.
Objects have identity, however identityHash is not unique.
The approach here is to assign the id during graph serialisation, alternately the id could be assigned using a counter at initialization.'!

!PrimitiveUgen methodsFor: 'initialization'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    name := nm. numChan := nc. rate := rt. specialIndex := ix. inputArray := in!
!

!PrimitiveUgen class methodsFor: 'instance creation'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    ^self new name: nm numChan: nc rate: rt specialIndex: ix inputArray: in!
!

!PrimitiveUgen methodsFor: 'accessing'!
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
addToUgenSet: set
    set add: self.
    inputArray do: [ :item | item addToUgenSet: set ]!
!

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
    ')' putOn: fd!
!
