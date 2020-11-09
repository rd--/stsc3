SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
 instanceVariableNames: 'paramNames rate numChan'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!UGen methodsFor: 'private'!
 setParamNames: x paramNames := x !
 setRate: x rate := x !
 setNumChan: x numChan := x ! !

!UGen methodsFor: 'accessing'!
 paramNames ^paramNames !
 rate ^rate !
 numChan ^numChan ! !

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
