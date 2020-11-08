SystemOrganization addCategory: #'SC3'!

Object subclass: #UGen
  instanceVariableNames: 'rate numChan'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'SC3' !

!UGen methodsFor: 'private'! setRate: r rate := r ! !
!UGen methodsFor: 'accessing'! rate ^rate ! !

Object subclass: #OutputProxy
instanceVariableNames: 'source outputIndex' classVariableNames: '' poolDictionaries: '' category: 'SC3' !

!OutputProxy methodsFor: 'private'!
setSource: s setOutputIndex: i source := s . outputIndex := i ! !

!OutputProxy class methodsFor: 'instance creation'!
source: source outputIndex: outputIndex ^self basicNew setSource: source setOutputIndex: outputIndex ! !

