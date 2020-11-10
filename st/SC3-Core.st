SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'name paramNames mceNames numChan rate'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!UGen methodsFor: 'private'!
 setName: x name := x !
 setParamNames: x paramNames := x !
 setMceNames: x mceNames := x !
 setNumChan: x numChan := x !
 setRate: x rate := x ! !

!UGen methodsFor: 'accessing'!
 name ^name !
 paramNames ^paramNames !
 mceNames ^mceNames !
 numChan ^numChan !
 rate ^rate ! !

Object subclass: #OutputProxy
 instanceVariableNames: 'source outputIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!OutputProxy methodsFor: 'private'!
 setSource: x source := x !
 setOutputIndex: x outputIndex := x ! !

!OutputProxy class methodsFor: 'instance creation'!
 source: source outputIndex: outputIndex ^self new setSource: source setOutputIndex: outputIndex ! !

Object subclass: #KR
 instanceVariableNames: 'source'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!KR methodsFor: 'private'!
 setSource: x source := x ! !

!KR class methodsFor: 'instance creation'!
 source: x ^self new setSource: x ! !
