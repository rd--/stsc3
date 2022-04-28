AbstractUgen subclass: #Ugen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId paramValues mceValues'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Ugen comment: 'Unit Generator.
Objects have identity, however identityHash is not unique.
The approach here is to assigned ugenId during graph serialisation, alternately the Id could be assigned using a counter at initialization.
All Ugens have mul: and add: methods but there are no variables associated with these.
'!

 !Ugen methodsFor: 'accessing'!
ugenName ^ugenName!
ugenName: ugenNameValue ugenName := ugenNameValue!
paramNames ^paramNames!
paramNames: paramNamesValue paramNames := paramNamesValue!
mceNames ^mceNames!
mceNames: mceNamesValue mceNames := mceNamesValue!
numChan ^numChan!
numChan: numChanValue numChan := numChanValue!
ugenRate ^ugenRate!
ugenRate: ugenRateValue ugenRate := ugenRateValue!
specialIndex ^specialIndex!
specialIndex: specialIndexValue specialIndex := specialIndexValue!
ugenId ^ugenId!
ugenId: ugenIdValue ugenId := ugenIdValue!
paramValues ^paramValues!
paramValues: paramValuesValue paramValues := paramValuesValue!
mceValues ^mceValues!
mceValues: mceValuesValue mceValues := mceValuesValue!
!

!Ugen methodsFor: 'collecting'!
addToUgenSet: aSet
    | addFunc |
    addFunc := [ :item | item addToUgenSet: aSet ].
    aSet add: self.
    paramValues do: addFunc.
    mceValues do: addFunc.
!
!

!Ugen methodsFor: 'arithmetic'!
mul: x
     "((p mul: q) mul: r) is (p * q * r) not (p * r)."
      ^x = 1 ifTrue: [ self ] ifFalse: [ self * x ]
!
add: x
     "((p add: q) add: r) is (p + q + r) not (p + r)."
      ^x = 0 ifTrue: [ self ] ifFalse: [ self + x ]
!
mul: mul add: add
    ^(self mul: mul) add: add
!
mulAdd: mul value: add
    "Alias for mul:add:.  (For .stc)"
    ^self mul: mul add: add
!
!
