Object subclass: #ScUgen
  instanceVariableNames: 'name numChan rate specialIndex id inputArray mrg'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

ScUgen comment: 'SuperCollider unit generator.
The id field is used to sort unit generators into applicative order.'!

!ScUgen methodsFor: 'initialization'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    name := nm.
    numChan := nc.
    rate := rt.
    specialIndex := ix.
    id := Sc3Id nextId.
    inputArray := in.
    mrg := Set new!
!

!ScUgen class methodsFor: 'instance creation'!
name: nm numChan: nc rate: rt specialIndex: ix inputArray: in
    ^self new name: nm numChan: nc rate: rt specialIndex: ix inputArray: in!
!

!ScUgen methodsFor: 'accessing'!
id
    ^id!
id: aNumber
    id := aNumber!
rate
    ^rate!
rate: aNumber
    rate := aNumber!
!

!ScUgen methodsFor: 'rate control'!
kr
    (rate = Rate ar) ifTrue: [rate := Rate kr].
    inputArray kr.
    ^self!
!

!ScUgen methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    (w includes: self) ifFalse: [
        c add: self.
        inputArray do: [ :item | item addToUgenSet: c protectedBy: w ].
        mrg do: [ :item | item addToUgenSet: c protectedBy: c ] ]!
!

!ScUgen methodsFor: 'mrg'!
mrg: rhs
    rhs asArray do: [ :item | mrg add: item ].
    ^self.
!

"Mrg needs to be printed here.
It can't be at printHaskellOn because that will be used on the lhs of =, and at printHaskellDefinitionOn: it makes a loop.
It requires an extra id, so another node, so Mrg...
Should just print a text syndef."
!ScUgen methodsFor: 'printing'!
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
