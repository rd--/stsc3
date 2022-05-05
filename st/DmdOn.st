PsuedoUgen subclass: #DmdOn
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

DmdOn comment: 'Demand with trigger input (Demand).'!

!DmdOn class methodsFor: 'instance creation'!
trig: trig reset: reset demandUGens: demandUGens
    ^Demand trig: trig reset: reset demandUGens: demandUGens!
primaryFactoryMethod
    ^#trig:reset:demandUGens:!
!
