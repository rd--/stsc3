SinOsc new setRate: 'ar' setFreq: 440 setPhase: 0

SinOsc rate: 'ar' freq: 440 phase: 0

SinOsc rate: 'ar' freq: (SinOsc rate: 'kr' freq: 2 phase: 0) phase: 0

o := (SinOsc rate: 'kr' freq: 2 phase: 0) setRate: 'ar'

Out rate: 'ar' bus: 0 in: (Pan2 rate: 'ar' in: (SinOsc rate: 'ar' freq: 440 phase: 0) pos: 0 level: 1)

