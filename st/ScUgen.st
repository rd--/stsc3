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
name
    ^name!
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
    ^self!
!

!ScUgen methodsFor: 'printing'!
printForGraph: aGraph on: aStream
    aStream
        << name << ' '
        << rate << ' '
        << inputArray size << ' '
        << numChan << ' '
        << specialIndex << ' '.
    inputArray do: [ :input | (aGraph ugenInputSpec: input) do: [ :item | aStream << item << ' ' ] ].
    numChan timesRepeat: [ aStream << rate << ' ' ]!
!
