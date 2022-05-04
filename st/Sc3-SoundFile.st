"This is not yet implemeted in stsc3, which currently has a stateless communication model for scsynth.
 The Sf cache is per scsynth session, it should be invalidated whenever scsynth is started or stopped."

Object subclass: #SfCache
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-SC3'
!

SfCache comment: 'Cache associating named sound files with allocated buffers at a synthesis server.'!

!SfCache methodsFor: 'naming'!
sfDictionary
    ^Dictionary newFrom: {
        'crotale-d6' -> '/home/rohan/sw/jssc3/flac/crotale-d6.flac'.
        'harp-a4' -> '/home/rohan/sw/jssc3/flac/harp-a4.flac'.
        'piano-c5' -> '/home/rohan/sw/jssc3/flac/piano-c5.flac'.
        'floating_1' -> '/home/rohan/sw/jssc3/flac/floating_1.flac'.
        'then' -> '/home/rohan/sw/jssc3/flac/then.flac' }
!!
