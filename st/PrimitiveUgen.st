AbstractUgen subclass: #PrimitiveUgen
  instanceVariableNames: 'ugenName paramNames mceNames numChan ugenRate specialIndex ugenId paramValues mceValues'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

PrimitiveUgen comment: 'Unit Generator.
Objects have identity, however identityHash is not unique.
The approach here is to assign ugenId during graph serialisation, alternately the Id could be assigned using a counter at initialization.'!

!PrimitiveUgen methodsFor: 'accessing'!
paramNameFromIndex: index
    "Get parameter name given index.
     Parameter names are separated into paramNames and mceNames.
     The implicit names mul: and add: follow."
    | n m |
    n := paramNames size.
    m := mceNames size.
    index <= n ifTrue: [^paramNames at: index].
    index <= (n + m) ifTrue: [^mceNames at: index - n].
    ^#(#mul #add) at: index - n - m!
!

!PrimitiveUgen methodsFor: 'accessing'!
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

!PrimitiveUgen methodsFor: '.stc'!
mulAdd: mul value: add
    "Alias for mul:add:.  (For .stc)"
    ^self mul: mul add: add
!
!

!PrimitiveUgen methodsFor: 'collecting'!
addToUgenSet: aSet
    | addFunc |
    addFunc := [ :item | item addToUgenSet: aSet ].
    aSet add: self.
    paramValues do: addFunc.
    mceValues do: addFunc.
!
!

!PrimitiveUgen methodsFor: 'printing'!
printHaskellOn: fd
    "Print the name I am bound to in hsc3."
    '_u_', ugenId asString putOn: fd
  !
printHaskellDefinitionOn: fd
    "Print my hsc3 definition."
    | mce nc |
    '(mkUGen Nothing all_rates ' putOn: fd.
    ugenRate isArray
      ifTrue: ['(Right ' putOn: fd. ugenRate printHaskellArrayOn: fd. ') ' putOn: fd]
      ifFalse: ['(Left ' putOn: fd. ugenRate asString putOn: fd. ') ' putOn: fd].
    ugenName asString printHaskellOn: fd.
    fd space.
    '[' putOn: fd.
    paramValues do: [ :item | item printHaskellOn: fd. fd space ] separatedBy: [ ',' putOn: fd ].
    '] ' putOn: fd.
    mce := ['(Just [' putOn: fd.
            mceValues do: [ :item | item printHaskellOn: fd. fd space ] separatedBy: [ ',' putOn: fd].
            ']) ' putOn: fd].
    mceNames isEmpty ifTrue: ['Nothing ' putOn: fd] ifFalse: mce.
    nc := ['(length (mceChannels ' putOn: fd.
           (mceValues at: 1) printHaskellOn: fd.
           '))' putOn: fd].
    numChan = #mceInput ifTrue: nc ifFalse: [numChan asString putOn: fd].
    fd space.
    '(Special ' putOn: fd.
    specialIndex notNil ifTrue: [specialIndex asString putOn: fd] ifFalse: ['0' putOn: fd]
  . ') ' putOn: fd.
    '(UId ' putOn: fd.
    ugenId asString putOn: fd.
    ')' putOn: fd.
    ')' putOn: fd.
  !
!




