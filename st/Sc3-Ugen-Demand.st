Ugen subclass: #DmdFor instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

DmdFor comment: 'Demand with duration input (Duty).'!

!DmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^Duty dur: dur reset: reset doneAction: 0 level: level
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!

Ugen subclass: #TDmdFor instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

TDmdFor comment: 'Demand with duration input (Duty).'!

!TDmdFor class methodsFor: 'instance creation'!
dur: dur reset: reset level: level
    ^TDuty dur: dur reset: reset doneAction: 0 level: level gapFirst: 0
!
primaryFactoryMethod
    ^#dur:reset:level:
!
!

Demand subclass: #DmdOn instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!
DmdOn comment: 'Demand with trigger input (Demand).'!

Drand subclass: #Choose instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!
Choose comment: 'Random sequence generator.'!

Dseq subclass: #Seq instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!
Seq comment: 'Sequence generator.'!

Dser subclass: #Ser instanceVariableNames: 'length start step' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!
Ser comment: 'Demand rate arithmetic series Ugen.'!

Dshuf subclass: #Shuf instanceVariableNames: 'repeats list' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!
Shuf comment: 'Demand rate random sequence generator'!
