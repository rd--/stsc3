Object subclass: #OutputProxy
 instanceVariableNames: 'source outputIndex'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !
!OutputProxy commentStamp: '<historical>' prior: 0! Output Proxy !

!OutputProxy methodsFor: 'accessing'!
 outputIndex ^ outputIndex !
 outputIndex: x outputIndex := x !
 source ^ source !
 source: x source := x !
!

!OutputProxy class methodsFor: 'instance creation'!
 source: source outputIndex: outputIndex ^self new source: source outputIndex: outputIndex ! !
