Object subclass: #EnvPerc
 instanceVariableNames: 'attackTime releaseTime level curve'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!EnvPerc methodsFor: 'accessing'!
attackTime ^ attackTime !
attackTime: anObject attackTime := anObject. !
curve ^ curve !
curve: anObject curve := anObject. !
level ^ level !
level: anObject level := anObject. !
releaseTime ^ releaseTime !
releaseTime: anObject releaseTime := anObject. ! !

!EnvPerc class methodsFor: 'instance creation'!
attackTime: p1 releaseTime: p2 level: p3 curve: p4
|u| u := self new . u attackTime: p1 . u releaseTime: p2 . u level: p3 . u curve: p4 . ^ u ! !

!EnvPerc methodsFor: 'envelope coordinates'!
coord ^ {{0 . level . 0} . {attackTime . releaseTime} . {curve}} ! !

Object subclass: #EnvLinen
 instanceVariableNames: 'attackTime sustainTime releaseTime level curve'
 classVariableNames: ''
 poolDictionaries: ''
 category: 'SC3' !

!EnvLinen methodsFor: 'accessing'!
attackTime ^ attackTime !
attackTime: anObject attackTime := anObject. !
curve ^ curve !
curve: anObject curve := anObject. !
level ^ level !
level: anObject level := anObject. !
releaseTime ^ releaseTime !
releaseTime: anObject releaseTime := anObject. !
sustainTime ^ sustainTime !
sustainTime: anObject sustainTime := anObject. ! !

!EnvLinen class methodsFor: 'instance creation'!
attackTime: p1 sustainTime: p2 releaseTime: p3 level: p4 curve: p5
|u| u := self new . u attackTime: p1 . u sustainTime: p2 . u releaseTime: p3 . u level: p4 . u curve: p5 . ^ u ! !

!EnvLinen methodsFor: 'envelope coordinates'!
coord ^ {{0 . level . level . 0} . {attackTime . sustainTime . releaseTime} . curve} ! !
