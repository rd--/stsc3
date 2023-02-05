# overlap -- texture generator

- _overlap(aProcedure, sustainTime, transitionTime, overlap)_

Schedules a periodic process on the _workspace::clock_ to evalute _aProcedure()_, which should construct a Ugen graph, and apply a self-releasing envelope with attack and release times equal to _transitionTime_ and a sustain time equal to _sustainTime_ to the answer, and sends the result to the synthesiser.

The process runs at the interval _sustainTime + (transitionTime * 2) / overlap_, so there will be at most _overlap_ number of instances of the sound playing together.
